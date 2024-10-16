using DataFrames, CSV, Plots


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

function step(agent::ExpectedModel, p::Float64)::DataFrame
  ret = DataFrame([p agent.αp agent.αn agent.q agent.ϵ response_strength(agent)],
                  ["p", "alpha.p", "alpha.n", "q", "epsilon", "r"])
  update_q(agent, p)
  update_ϵ(agent, p)
  return ret
end

function run(αp::Float64, αn::Float64, p::Float64)
  agent = ExpectedModel(αp, αn)
  reward_phase = map(_ -> step(agent, p), 1:200) |> x -> reduce(vcat, x)
  extinction_phase = map(_ -> step(agent, 0.), 1:200) |> x -> reduce(vcat, x)
  vcat(reward_phase, extinction_phase)
end

CSV.write("./data/fig1F_QHCM.csv", run(0.1, 0.01, 1.))
CSV.write("./data/fig1F_VQM.csv", run(0.1, 0.01, 1.))
