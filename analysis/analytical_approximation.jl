using Plots, DataFrames, CSV

function expected_q_epsilon(q::Real, epsilon::Real,
                            p::Real, t::Int64,
                            alpha_p::Real, alpha_n::Real, w::Real)
    qs = Array{Real, 1}([])
    epsilons = Array{Real, 1}([])
    thetas = Array{Real, 1}([])

    for _ in 1:t
        theta = (1 - w) * q + w * epsilon
        push!(qs, q)
        push!(epsilons, epsilon)
        push!(thetas, theta)
        delta_p = 1 - q
        delta_n = 0 - q
        q += alpha_p * p * delta_p + alpha_n * (1 - p) * delta_n
        v = (p * abs(delta_p) + (1 - p) * abs(delta_n)) - epsilon
        epsilon += alpha_p * v
    end

    return DataFrame(p = [p for _ in 1:t],
                     alpha_p = [alpha_p for _ in 1:t],
                     alpha_n = [alpha_n for _ in 1:t],
                     w = [w for _ in 1:t],
                     q = qs,
                     epsilon = epsilons,
                     theta = thetas)
end

function burst_strength(baseline::AbstractDataFrame, extinction::AbstractDataFrame)
    base_theta = last(baseline.theta)
    bursts = extinction[extinction.theta .>= base_theta, :theta]
    if length(bursts) > 0
        return sum(bursts .- base_theta)
    end
    return 0.
end

function calculate(p::Real, t::Int64,
                   alpha_p::Real, alpha_n::Real, w::Real)
    baseline = expected_q_epsilon(0, 0,
                                  p, t,
                                  alpha_p, alpha_n, w)
    extinction = expected_q_epsilon(last(baseline.q), last(baseline.epsilon),
                                    0.0, t,
                                    alpha_p, alpha_n, w)
    burst = DataFrame(p = p,
                      alpha_p = alpha_p,
                      alpha_n = alpha_n,
                      w = w,
                      burst = burst_strength(baseline, extinction))
    return vcat(baseline, extinction), burst
end

alpha_p_range = Array{Real, 1}(0.01:0.01:0.5)
alpha_n_range = Array{Real, 1}(0.01:0.01:0.5)
w_range = Array{Real, 1}(0.25:0.25:1.)
p = Array{Real, 1}(0.25:0.25:1.)

params = collect(Iterators.product(p, alpha_p_range, alpha_n_range, w_range))

# rawdata = Array{AbstractDataFrame, 1}([])
brstdara = Array{AbstractDataFrame, 1}([])

for (p, ap, an, w) in params
    _, b = calculate(p, 500, ap, an, w)
    # push!(rawdata, r)
    push!(brstdara, b)
end

brststr = reduce(vcat, brstdara)
CSV.write("./brststr.csv", brststr)

example, _ = calculate(0.5, 200, 0.4, 0.01, 0.5)

plot(example.q, label = "Qt")
plot!(example.epsilon, label = "εt")
plot!(example.theta, label = "θt")

CSV.write("./data/analytical_approximation.csv", example)
