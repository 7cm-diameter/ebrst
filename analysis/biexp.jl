using Turing, Distributions, Plots, StatsPlots, MCMCChains, DataFrames, CSV, Logging, ReverseDiff
# supress log diuring MCMC
# Logging.disable_logging(Logging.Warn)

IRT = Real
Reward = Real
QValue = Real
Curiosity = Real

mutable struct Agent
    q::QValue
    epsilon::Curiosity
    alpha_p::Real
    alpha_n::Real
    w::Real
    a::Real
end

function mixture_ratio(a::Real, w::Real, q::Real, epsilon::Real)
    return 1 - exp(-a * ((1 - w) * q + w * epsilon))
end

function mixture_ratio(agent::Agent)
    return mixture_ratio(agent.a, agent.w, agent.q, agent.epsilon)
end

function update(alpha_p::Real, alpha_n::Real,
                q::QValue, epsilon::Curiosity,
                reward::Reward)
    rpe = reward - q
    v = abs(rpe) - epsilon
    if rpe > 0
        q += alpha_p * rpe
    else
        q += alpha_n * rpe
    end
    if v > 0
        epsilon += alpha_p * v
    else
        epsilon += alpha_n * v
    end
end

function update(agent::Agent, reward:: Reward)
    update(agent.alpha_p, agent.alpha_n, agent.q, agent.epsilon, reward)
end

@model HierarchicalQLearning(IRTs::Array{IRT, 1}, rewards::Array{Reward, 1}) = begin
    T = length(IRTs)
    q ~ Beta(1, 1)
    epsilon ~ Beta(1, 1)
    alpha_p ~ Beta(1, 1)
    alpha_n ~ truncated(Normal(), 0., alpha_p)
    # alpha_n ~ Beta(1, 1)
    w ~ Beta(1, 1)
    a ~ Gamma(1, 100)
    theta_w ~ Gamma(1, 10)
    theta_b ~ truncated(Normal(), theta_w, 100)
    theta = [theta_w, theta_b]
    agent = Agent(q, epsilon, alpha_p, alpha_n, w, a)
    s = Vector{Int}(undef, T)

    for t in 1:T
        r = rewards[t]
        p = mixture_ratio(agent)
        s[t] ~ Categorical([p, 1 - p])
        IRTs[t] ~ Exponential(theta[s[t]])
        update(agent, r)
    end
end

fittable_data = CSV.read("./data/pilot/fittable.csv", DataFrame)
grouped_data = groupby(fittable_data, [:subject, :session])

for d in grouped_data[1:1]
    d = d[d.event .== 9, :]
    actions = Array{IRT, 1}([r for r in d.IRT])
    rewards = Array{Reward, 1}([r for r in d.reward])
    _ = sample(HierarchicalQLearning(actions, rewards), MH(), MCMCThreads(), 2, 4)
end

results = Array{Chains, 1}([])

for d in grouped_data[1:1]
    d = d[d.event .== 9, :]
    subject = d.subject |> unique
    session = d.session |> unique
    println([subject, session])
    actions = Array{IRT, 1}([r for r in d.IRT])
    rewards = Array{Reward, 1}([r for r in d.reward])
    println("Hierarchical")
    chains = sample(HierarchicalQLearning(actions, rewards), NUTS(), MCMCThreads(), 2000, 4)
    push!(results, chains)
end

results[1]
