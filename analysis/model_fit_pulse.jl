##################
# Standard Q-HCM #
##################
using Turing, CSV, DataFrames, StatsFuns, StatsBase, Plots, Distributed
addprocs(4)


mutable struct QHCM
  αqp::Real
  αqn::Real
  αv::Real
  w:: Real
  a::Real
  b::Real
  q::Real
  v::Real

  function QHCM(αqp::Real, αqn::Real, αv::Real, w::Real, a::Real, b::Real, q::Real, v::Real)
      new(αqp, αqn, αv, w, a, b, q, v)
  end
end

function pulse_rate(model::QHCM)
  x = model.w * model.v + (1 - model.w) * model.q
  1 / (1 + exp(-(model.a * x + model.b)))
end

function update(model::QHCM, reward::Real)
  δ = reward - model.q
  ϵ = abs(δ) - model.v
  αq = ifelse(δ > 0, model.αqp, model.αqn)
  model.q += αq * δ
  model.v += model.αv * ϵ
end

function waic(model, chain)
  logmeanexp(x) = logsumexp(x) - log(length(x))
  model_params = chain.name_map[:parameters]
  lppd = pointwise_loglikelihoods(model, chain[model_params])
  lppd = values(lppd)
  pointwise_waic = -2*(logmeanexp.(lppd) - var.(lppd))
  return sum(pointwise_waic)
end

################
# Turing model #
################

@model QHCMTL(actions, rewards::Vector{Real}, blocks::Vector{Real}) = begin
  T = length(actions)

  # Shared across all blocks
  αqp ~ Beta(1, 1)
  αqn ~ Beta(1, 1)
  αv ~ Beta(1, 1)
  w ~ Beta(1, 1)
  a ~ truncated(Normal(), 0, 100)
  b ~ Normal()

  # Different for each block
  # q ~ truncated(Normal(), 0., 1.)
  # v ~ truncated(Normal(), 0., 1.)
  q ~ Beta(1, 1)
  v ~ Beta(1, 1)

  m = QHCM(αqp, αqn, αv, w, a, b, q, v)

  for t in 1:T
    reward = rewards[t]
    actions[t] ~ Bernoulli(pulse_rate(m))
    update(m, reward)
  end
end


@model VQMTL(actions, rewards::Vector{Real}, blocks::Vector{Real}) = begin
  T = length(actions)

  # Shared across all blocks
  αqp ~ Beta(1, 1)
  αqn ~ Beta(1, 1)
  αv = 0.
  w = 0.
  a ~ truncated(Normal(), 0, 100)
  b ~ Normal()

  # q ~ truncated(Normal(), 0., 1.)
  q ~ Beta(1, 1)
  v = 0.

  m = QHCM(αqp, αqn, αv, w, a, b, q, v)

  for t in 1:T
    reward = rewards[t]
    actions[t] ~ Bernoulli(pulse_rate(m))
    update(m, reward)
  end
end

function predict_qhcm(result, rewards::Vector{Real})
  T = length(rewards)
  params = mean(result)
  m = QHCM(params[:αqp, :mean], params[:αqn, :mean], params[:αv, :mean],
                 params[:w, :mean], params[:a, :mean], params[:b, :mean],
                 params[:q, :mean], params[:v, :mean])
  q = Vector{Real}(undef, T)
  v = Vector{Real}(undef, T)
  p = Vector{Real}(undef, T)

  for t in 1:T
    q[t] = m.q
    v[t] = m.v
    p[t] = pulse_rate(m)
    update(m, rewards[t])
  end

  DataFrame(q = q, v = v, p = p)
end

function predict_vqm(result, rewards::Vector{Real})
  T = length(rewards)
  params = mean(result)
  m = QHCM(params[:αqp, :mean], params[:αqn, :mean], 0.,
                 0., params[:a, :mean], params[:b, :mean],
                 params[:q, :mean], 0.)
  q = Vector{Real}(undef, T)
  p = Vector{Real}(undef, T)

  for t in 1:T
    q[t] = m.q
    p[t] = pulse_rate(m)
    update(m, rewards[t])
  end

  DataFrame(q = q, p = p)
end


###################
# Fitting to data #
###################

paths = readdir("./data", join = true) |>
  filter(path -> occursin("WT", path) & occursin("subses.csv", path))

for path in paths
  d = CSV.read(path, DataFrame)
  subject = d.subject |> unique |> x -> x[1]
  session = d.session |> unique |> x -> x[1]
  condition = d.condition |> unique |> x -> x[1]

  actions = Vector{Real}(d.pulse)
  rewards = Vector{Real}(d.reward)
  blocks = Vector{Real}(d.block)

  qhcm_model = QHCMTL(actions, rewards, blocks)
  qhcm_result = sample(qhcm_model, NUTS(), MCMCThreads(), 4000, 4, discard_initial=3000)

  vqm_model = VQMTL(actions, rewards, blocks)
  vqm_result = sample(vqm_model, NUTS(), MCMCThreads(), 4000, 4, discard_initial=3000)

  qhcm_waic = waic(qhcm_model, qhcm_result)
  vqm_waic = waic(vqm_model, vqm_result)

  qhcm_prediction = predict_qhcm(qhcm_result, rewards)
  vqm_prediction = predict_vqm(vqm_result, rewards)

  d.q_qhcm = qhcm_prediction.q
  d.v_qhcm = qhcm_prediction.v
  d.p_qhcm = qhcm_prediction.p

  d.q_vqm = vqm_prediction.q
  d.p_vqm = vqm_prediction.p

  params = DataFrame(mean(qhcm_result))
  params.subject = repeat([subject], nrow(params))
  params.session = repeat([session], nrow(params))
  params.condition = repeat([condition], nrow(params))

  WAIC_data = DataFrame(subject = repeat([subject], 2),
                        session = repeat([session], 2),
                        condition = repeat([condition], 2),
                        model = ["Q-HCM", "VQM"],
                        waic = [qhcm_waic, vqm_waic])

  CSV.write("./data/" * subject * "-" * string(session) * "_subses_fitted.csv", d)
  CSV.write("./data/" * subject * "-" * string(session) * "_subses_params.csv", params)
  CSV.write("./data/" * subject * "-" * string(session) * "_subses_waic.csv", WAIC_data)
end
