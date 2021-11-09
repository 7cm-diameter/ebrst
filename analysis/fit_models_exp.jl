using Turing, Distributions, Plots, StatsPlots, MCMCChains, DataFrames, CSV, Logging
Logging.disable_logging(Logging.Warn)

Action = Real
Reward = Real
Probability = Real
QValue = Real
Curiosity = Real

mutable struct Agent
    q::QValue
    v::Curiosity
    alpha_p::Real
    alpha_m::Real
    w::Real
    a::Real
end

function response_strength(agent::Agent)
    1 / (agent.a * ((1 - agent.w) * agent.q + agent.w * agent.v))
end

function update(agent::Agent, reward::Reward)
    err = reward - agent.q
    v = (abs(err) - agent.v)
    if err > 0
        agent.q += agent.alpha_p * err
    else
        agent.q += agent.alpha_m * err
    end
    if v > 0
        agent.v += agent.alpha_p * v
    else
        agent.v += agent.alpha_m * v
    end
end

exponential_ll(theta, IRT) = loglikelihood(Exponential(theta), IRT)

@model HierarchicalQLearning(action::Array{Action, 1}, reward::Array{Reward, 1}) = begin
    q ~ truncated(Normal(), 0.01, 1.)
    v ~ truncated(Normal(), 0.01, 1.)
    alpha_p ~ Beta(1, 1)
    alpha_m ~ truncated(Normal(), 0., alpha_p)
    w ~ Beta(1, 1)
    a ~ truncated(Normal(), 0.01, 100.)
    agent = Agent(q, v, alpha_p, alpha_m, w, a)

    for t in 1:length(action)
        a = action[t]
        r = reward[t]
        theta = response_strength(agent)
        action[t] ~ Exponential(theta)
        # Turing.@addlogprob! exponential_ll(theta, a)
        update(agent, r)
    end
end

@model VanillaQLearning(action::Array{Action, 1}, reward::Array{Reward, 1}) = begin
    # `v` and `w` is fixed to reduce HierarchicalQLearning to vanilla Q-learning
    v = 0.
    w = 0.
    q ~ truncated(Normal(), 0.01, 1.)
    alpha_p ~ Beta(1, 1)
    alpha_m ~ truncated(Normal(), 0., alpha_p)
    a ~ truncated(Normal(), 0.01, 100.)
    agent = Agent(q, v, alpha_p, alpha_m, w, a)

    for t in 1:length(action)
        a = action[t]
        r = reward[t]
        theta = response_strength(agent)
        action[t] ~ Exponential(theta)
        update(agent, r)
    end
end

# model selection
function expll(theta::Real, IRT::Real)
    return log(pdf(Exponential(theta), IRT))
end

function hqlll(agent::Agent, IRTs::Array{Action, 1})
    ll = 0.

    for IRT in IRTs
        theta = response_strength(agent)
        ll += expll(theta, IRT)
    end

    return ll
end

function vqlll(agent::Agent, IRTs::Array{Action, 1})
    ll = 0.

    for IRT in IRTs
        theta = response_strength(agent)
        ll += expll(theta, IRT)
    end

    return ll
end

function summarize(data::AbstractDataFrame, hql::Chains, vql::Chains)
    IRTs = Array{Action, 1}(data.IRT)

    hqlq = mean(hql["q"])
    hqlv = mean(hql["v"])
    hql_alpha_p = mean(hql["alpha_p"])
    hql_alpha_m = mean(hql["alpha_m"])
    hqlw = mean(hql["w"])
    hqla = mean(hql["a"])

    hqlAIC = -hqlll(Agent(hqlq, hqlv, hql_alpha_p, hql_alpha_m, hqlw, hqla), IRTs) + 2 * 7

    vqlq = mean(vql["q"])
    vqlv = 0.
    vql_alpha_p = mean(vql["alpha_p"])
    vql_alpha_m = mean(vql["alpha_m"])
    vqlw = 0.
    vqla = mean(vql["a"])

    vqlAIC = -vqlll(Agent(vqlq, vqlv, vql_alpha_p, vql_alpha_m, vqlw, vqla), IRTs) + 2 * 4

    subject = unique(data.subject)[1]
    condition = unique(data.condition)[1]
    session = unique(data.session)[1]
    DataFrame(subject = [subject for _ in 1:2],
              condition = [condition for _ in 1:2],
              session = [session for _ in 1:2],
              model = ["heirarchical", "vanilla"],
              q = [hqlq, vqlq],
              v = [hqlv, vqlv],
              alpha_p = [hql_alpha_p, vql_alpha_p],
              alpha_m = [hql_alpha_m, vql_alpha_m],
              w = [hqlw, vqlw],
              a = [hqla, vqla],
              AIC = [hqlAIC, vqlAIC])
end

# calculate estimated response rate
function calculate_response_rate(data::AbstractDataFrame, hql::Chains, vql::Chains)
    hqlq = mean(hql["q"])
    hqlv = mean(hql["v"])
    hql_alpha_p = mean(hql["alpha_p"])
    hql_alpha_m = mean(hql["alpha_m"])
    hqlw = mean(hql["w"])
    hqla = mean(hql["a"])
    hql_agent = Agent(hqlq, hqlv, hql_alpha_p, hql_alpha_m, hqlw, hqla)

    vqlq = mean(vql["q"])
    vqlv = 0.
    vql_alpha_p = mean(vql["alpha_p"])
    vql_alpha_m = mean(vql["alpha_m"])
    vqlw = 0.
    vqla = mean(vql["a"])
    vql_agent = Agent(vqlq, vqlv, vql_alpha_p, vql_alpha_m, vqlw, vqla)

    hql_theta = Array{Real, 1}([])
    vql_theta = Array{Real, 1}([])

    rewards = Array{Reward, 1}(data.reward)
    events = Array{Int64, 1}(data.event)

    for (r, e) in zip(rewards, events)
        if e == 9
            push!(hql_theta, response_strength(hql_agent))
            push!(vql_theta, response_strength(vql_agent))
            update(hql_agent, r)
            update(vql_agent, r)
        else
            push!(hql_theta, 0)
            push!(vql_theta, 0)
        end
    end

    theta = DataFrame(hql_theta = hql_theta, vql_theta = vql_theta)
    return hcat(data, theta)
end

# run analysis
fittable_data = CSV.read("./data/pilot/fittable.csv", DataFrame)
grouped_data = groupby(fittable_data, [:subject, :session])

for d in grouped_data[1:1]
    d = d[d.event .== 9, :]
    actions = Array{Action, 1}([r for r in d.IRT])
    rewards = Array{Reward, 1}([r for r in d.reward])
    _ = sample(HierarchicalQLearning(actions, rewards), NUTS(), MCMCThreads(), 2, 4)
    _ = sample(VanillaQLearning(actions, rewards), NUTS(), MCMCThreads(), 2, 4)
end

hql_results = Array{Chains, 1}([])
vql_results = Array{Chains, 1}([])

for d in grouped_data[1:3]
    d = d[d.event .== 9, :]
    subject = d.subject |> unique
    session = d.session |> unique
    println([subject, session])
    actions = Array{Action, 1}([r for r in d.IRT])
    rewards = Array{Reward, 1}([r for r in d.reward])
    println("Hierarchical")
    hql_chains = sample(HierarchicalQLearning(actions, rewards), NUTS(), MCMCThreads(), 2000, 4)
    push!(hql_results, hql_chains)
    println("Vanilla")
    vql_chains = sample(VanillaQLearning(actions, rewards), NUTS(), MCMCThreads(), 2000, 4)
    push!(vql_results, vql_chains)
end

summary = Array{AbstractDataFrame, 1}([])

for (d, h, v) in zip(grouped_data, hql_results, vql_results)
    d = d[d.event .== 9, :]
    push!(summary, summarize(d, h, v))
end

summary = reduce(vcat, summary)
CSV.write("./data/pilot/fitted_result.csv", summary)

estimated_data = Array{AbstractDataFrame, 1}([])

for (d, h, v) in zip(grouped_data, hql_results, vql_results)
    push!(estimated_data, calculate_response_rate(d, h, v))
end

d = estimated_data[3]
plot(1 ./ d[d.event .== 9, :].IRT)
plot!(1 ./ d[d.event .== 9, :].hql_theta)
plot!(1 ./ d[d.event .== 9, :].vql_theta)

estimated_data = reduce(vcat, estimated_data)

CSV.write("./data/pilot/estimated_data.csv", estimated_data)
