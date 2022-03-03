###
# Authors: Daniel Thuerck
# NEC Laboratories Europe GmbH, Copyright (c) 2021, All rights reserved.
#
# THIS HEADER MAY NOT BE EXTRACTED OR MODIFIED IN ANY WAY.
#
# SOFTWARE LICENSE AGREEMENT
# ACADEMIC OR NON-PROFIT ORGANIZATION NONCOMMERCIAL RESEARCH USE ONLY
# BY USING OR DOWNLOADING THE SOFTWARE, YOU ARE AGREEING TO THE TERMS
# OF THIS LICENSE AGREEMENT. IF YOU DO NOT AGREE WITH THESE TERMS, YOU MAY
# NOT USE OR DOWNLOAD THE SOFTWARE.
#
# This is a license agreement ("Agreement") between your academic institution or non-profit
# organization or self (called "Licensee" or "You" in this Agreement) and NEC Laboratories Europe
# GmbH (called "Licensor" in this Agreement). All rights not specifically granted to you in this
# Agreement are reserved for Licensor.
#
# RESERVATION OF OWNERSHIP AND GRANT OF LICENSE: Licensor retains exclusive
# ownership of any copy of the Software (as defined below) licensed under this Agreement and hereby
# grants to Licensee a personal, non-exclusive, non-transferable license to use the Software for
# noncommercial research purposes, without the right to sublicense, pursuant to the terms and
# conditions of this Agreement. NO EXPRESS OR IMPLIED LICENSES TO ANY OF LICENSOR’S
# PATENT RIGHTS ARE GRANTED BY THIS LICENSE. As used in this Agreement, the term
# "Software" means (i) the actual copy of all or any portion of code for program routines made
# accessible to Licensee by Licensor pursuant to this Agreement, inclusive of backups, updates, and/or
# merged copies permitted hereunder or subsequently supplied by Licensor, including all or any file
# structures, programming instructions, user interfaces and screen formats and sequences as well as any
# and all documentation and instructions related to it, and (ii) all or any derivatives and/or modifications
# created or made by You to any of the items specified in (i).
#
# CONFIDENTIALITY/PUBLICATIONS: Licensee acknowledges that the Software is proprietary to
# Licensor, and as such, Licensee agrees to receive all such materials and to use the Software only in
# accordance with the terms of this Agreement. Licensee agrees to use reasonable effort to protect the
# Software from unauthorized use, reproduction, distribution, or publication. All publication materials
# mentioning features or use of this software must explicitly include an acknowledgement the software
# was developed by NEC Laboratories Europe GmbH.
#
# COPYRIGHT: The Software is owned by Licensor.
#
# PERMITTED USES: The Software may be used for your own noncommercial internal research
# purposes. You understand and agree that Licensor is not obligated to implement any suggestionsand/or 
# feedback you might provide regarding the Software, but to the extent Licensor does so, you are
# not entitled to any compensation related thereto.
#
# DERIVATIVES: You may create derivatives of or make modifications to the Software, however, You
# agree that all and any such derivatives and modifications will be owned by Licensor and become a part
# of the Software licensed to You under this Agreement. You may only use such derivatives and
# modifications for your own noncommercial internal research purposes, and you may not otherwise
# use, distribute or copy such derivatives and modifications in violation of this Agreement.
#
# BACKUPS: If Licensee is an organization, it may make that number of copies of the Software
# necessary for internal noncommercial use at a single site within its organization provided that all
# information appearing in or on the original labels, including the copyright and trademark notices are
# copied onto the labels of the copies.
#
# USES NOT PERMITTED: You may not distribute, copy or use the Software except as explicitly
# permitted herein. Licensee has not been granted any trademark license as part of this Agreement.
# Neither the name of NEC Laboratories Europe GmbH nor the names of its contributors may be used to
# endorse or promote products derived from this Software without specific prior written permission.
# You may not sell, rent, lease, sublicense, lend, time-share or transfer, in whole or in part, or provide
# third parties access to prior or present versions (or any parts thereof) of the Software.
#
# ASSIGNMENT: You may not assign this Agreement or your rights hereunder without the prior
# written consent of Licensor. Any attempted assignment without such consent shall be null and void.
#
# TERM: The term of the license granted by this Agreement is from Licensee's acceptance of this
# Agreement by downloading the Software or by using the Software until terminated as provided below.
# The Agreement automatically terminates without notice if you fail to comply with any provision of
# this Agreement. Licensee may terminate this Agreement by ceasing using the Software. Upon any
# termination of this Agreement, Licensee will delete any and all copies of the Software. You agree that
# all provisions which operate to protect the proprietary rights of Licensor shall remain in force should
# breach occur and that the obligation of confidentiality described in this Agreement is binding in
# perpetuity and, as such, survives the term of the Agreement.
#
# FEE: Provided Licensee abides completely by the terms and conditions of this Agreement, there is no
# fee due to Licensor for Licensee's use of the Software in accordance with this Agreement.
#
# DISCLAIMER OF WARRANTIES: THE SOFTWARE IS PROVIDED "AS-IS" WITHOUT
# WARRANTY OF ANY KIND INCLUDING ANY WARRANTIES OF PERFORMANCE OR
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE OR PURPOSE OR OF NON-
# INFRINGEMENT. LICENSEE BEARS ALL RISK RELATING TO QUALITY AND
# PERFORMANCE OF THE SOFTWARE AND RELATED MATERIALS.
#
# SUPPORT AND MAINTENANCE: No Software support or training by the Licensor is provided as
# part of this Agreement.
#
# EXCLUSIVE REMEDY AND LIMITATION OF LIABILITY: To the maximum extent permitted
# under applicable law, Licensor shall not be liable for direct, indirect, special, incidental, or
# consequential damages or lost profits related to Licensee's use of and/or inability to use the Software,
# even if Licensor is advised of the possibility of such damage.
#
# EXPORT REGULATION: Licensee agrees to comply with any and all applicable export control laws,
# regulations, and/or other laws related to embargoes and sanction programs administered by law.
#
# SEVERABILITY: If any provision(s) of this Agreement shall be held to be invalid, illegal, or
# unenforceable by a court or other tribunal of competent jurisdiction, the validity, legality and
# enforceability of the remaining provisions shall not in any way be affected or impaired thereby.
#
# NO IMPLIED WAIVERS: No failure or delay by Licensor in enforcing any right or remedy under this
# Agreement shall be construed as a waiver of any future or other exercise of such right or remedy by
# Licensor.
#
# GOVERNING LAW: This Agreement shall be construed and enforced in accordance with the laws of
# Germany without reference to conflict of laws principles. You consent to the personal jurisdiction of
# the courts of this country and waive their rights to venue outside of Germany.
#
# ENTIRE AGREEMENT AND AMENDMENTS: This Agreement constitutes the sole and entire
# agreement between Licensee and Licensor as to the matter set forth herein and supersedes any
# previous agreements, understandings, and arrangements between the parties relating hereto.
#
# THIS HEADER MAY NOT BE EXTRACTED OR MODIFIED IN ANY WAY.
###

module example

function ex1()
    println("Hello from ex1!")
end

function ex2(a::Vector{Float64})
    println("a in ex2:")
    for i in 1:size(a, 1)
        println("a[", i, "] = ", a[i])
    end
end

mutable struct ex3s
    b::Vector{Float64}
end

mutable struct ex5s
    a::Int
    b::Vector{Float64}
end

function ex3(a::ex3s) :: Int
    println("b in ex3:")
    for i in 1:size(a.b, 1)
        println("a[", i, "] = ", a.b[i])
    end

    return Int(1.0)
end

function ex4(a::Tuple{Int64, Array{Float64, 2}}) :: Vector{Float64}
    println("c in ex4:")
    for i in 1:size(a[2], 1)
        for j in 1:size(a[2], 2)
            println("a[2][", i, ", ", j, "] = ", a[2][i, j])
        end
    end

    println("a[1] = ", a[1])

    ff = zeros(Float64, size(a[2], 1) * size(a[2], 2))

    ix = 1
    for i in 1:size(a[2], 1)
        for j in 1:size(a[2], 2)
            ff[ix] = a[2][i, j]
            ix = ix + 1
        end
    end

    return ff
end

function ex5() :: ex5s
    ff = zeros(Float64, 10)
    for i in 1:size(ff, 1)
        ff[i] = -i
    end

    tt = ex5s(1337, ff)

    return tt
end

function ex6() :: Tuple{Int64, Vector{Float64}}
    ff = zeros(Float64, 10)
    for i in 1:size(ff, 1)
        ff[i] = -i
    end

    return (1337, ff)
end

end