using DataFrames, CSV


mutable struct ExpectedModel
  αp::Float64
  αn::Float64
  q::Float64
  ϵ::Float64

  function ExpectedModel(αp::Float64, αn::Float64)
      new(αp, αn, 0., 0.)
  end
end

function update_q(agent::ExpectedModel, p::Float64)
  agent.q += p * agent.αp * (1 - agent.q) + (1. - p) * agent.αn * (0 - agent.q)
end

function update_ϵ(agent::ExpectedModel, p::Float64)
  agent.ϵ += agent.αp * (p * (1 - agent.q) + (1 - p) * agent.q - agent.ϵ)
end

function response_strength(agent::ExpectedModel)::Float64
  return agent.q + agent.ϵ
end

function step(agent::ExpectedModel, p::Float64)
  update_q(agent, p)
  update_ϵ(agent, p)
end

function run(parameters::Tuple{Float64, Float64, Float64})::DataFrame
  p, αp, αn = parameters
  agent = ExpectedModel(αp, αn)
  δ = Inf
  while δ >= 1e-5
    q = agent.q
    step(agent, p)
    δ = agent.q - q
  end
  baseline = response_strength(agent)

  burts = Vector{Float64}([0.])
  while response_strength(agent) - baseline >= 0
    push!(burts, response_strength(agent) - baseline)
    step(agent, 0.)
  end

  DataFrame([p αp αn sum(burts)], ["p", "alpha.p", "alpha.n", "burst"])
end

Αp = collect(0.01:0.001:.5)
Αn = collect(0.1:0.1:1.)
P = collect(0.01:0.001:1.)

parameter_sets = Iterators.product(P, Αp, Αn) |> collect |> x -> reshape(x, 1, :)
results = map(params -> run(params), parameter_sets) |> x -> reduce(vcat, x)
CSV.write("./data/fig1G.csv", results)
