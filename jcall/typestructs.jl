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

# list of symbols for primitive types
supported_continuous_type_symbols = [
    :Float16,
    :Float32,
    :Float64
]
supported_int_type_symbols = [
    :Int128,
    :Int16,
    :Int32,
    :Int64,
    :Int8,
    :UInt128,
    :UInt16,
    :UInt32,
    :UInt64,
    :UInt8,
    :Bool
]
unsupported_type_symbols = [
    :Integer,
    :Signed,
    :Unsigned,
    :Complex,
    :Real,
    :Number,
    :AbstractFloat,
    :BigFloat,
    :BigInt
]

# simpicistic type hierachy (just mentions, not, e.g., struct definitions)
abstract type type_t end
abstract type primitive_t <: type_t end
abstract type composite_t <: type_t end

mutable struct continuous_t <: primitive_t
    bit_width
    complex
end

mutable struct integer_t <: primitive_t
    bit_width
    unsigned
end

mutable struct void_t <: primitive_t
end

mutable struct ptr_t <: composite_t
    pointee_t :: type_t
end

mutable struct identifier_t
    name :: Symbol
    parts :: Vector{Symbol}
    fqname
end

mutable struct struct_ref_t <: composite_t
    name :: identifier_t
end

mutable struct array_t <: composite_t
    pointee_t :: type_t
    dims
end

mutable struct param
    name
    type :: type_t
    managed :: Bool

    replacement
end

mutable struct tuple_t <: composite_t
    members :: Array{param}
end

# structs are defined seperately (other then tuples, they are not anonymous)
mutable struct struct_def <: composite_t
    name :: identifier_t
    members :: Array{param}
end

# dumping back to expr
function expr_from_type(t::continuous_t)
    return Symbol("Float" * string(t.bit_width))
end

function expr_from_type(t::integer_t)
    if t.bit_width == 1
        return Symbol("Bool")
    end
    sign = ""
    if t.unsigned
        sign = "U"
    end
    return Symbol(sign * "Int" * string(t.bit_width))
end

function expr_from_type(t::void_t)
    return Symbol("Nothing")
end

function expr_from_type(t::ptr_t)
    return Expr(
        Symbol("curly"),
        Symbol("Ptr"),
        expr_from_type(t.pointee_t)
    )
end

function expr_from_type(t::struct_ref_t)
    return t.name.name
end

function expr_from_type(t::array_t)
    args_array = []
    if t.dims == 1
        push!(args_array, Symbol("Vector"))
    else
        push!(args_array, Symbol("Array"))
    end

    push!(args_array, expr_from_type(t.pointee_t))

    if t.dims > 1
        push!(args_array, t.dims)
    end

    return Expr(
        Symbol("curly"),
        args_array...
    )
end

function expr_from_type(t::tuple_t)
    member_args = []
    for m in t.members
        push!(member_args, expr_from_type(m))
    end

    return Expr(
        Symbol("curly"),
        Symbol("Tuple"),
        member_args...
    )
end

# mapping to C-Type
function cstring_from_type(t::continuous_t)
    if t.bit_width == 16
        return "half"
    elseif t.bit_width == 32
        return "float"
    end

    return "double"
end

function cstring_from_type(t::integer_t)
    s = ""

    if t.bit_width == 8
        s *= "char"
    elseif t.bit_width == 16
        s *= "short"
    elseif t.bit_width == 32
        s *= "int"
    else
        s *= "long long"
    end

    if t.unsigned
        s = "unsigned " * s
    end

    return s
end

function cstring_from_type(t::void_t)
    return "void"
end

function cstring_from_type(t::ptr_t)
    return cstring_from_type(t.pointee_t) * " *"
end

function cstring_from_type(t::struct_ref_t)
    return string(t.name.name)
end

function cstring_from_type(t::array_t)
    return cstring_from_type(t.pointee_t) * "[]"
end

function cstring_from_type(t::tuple_t)
    # tuples must be mapped to anonymous structs
    return hash_type(t)
end

# handles names with "." in it
function parse_id(expr :: Symbol)
    return identifier_t(expr, [expr], expr)
end

function name_parts_from_expr(expr)
    if isa(expr, Symbol)
        return [expr]
    elseif isa(expr, QuoteNode)
        return [expr.value]
    elseif expr.head == Symbol(".")
        names = []

        for part in expr.args
            append!(
                names,
                name_parts_from_expr(part)
            )
        end

        return names
    else
        error("Unknown name expr: ", expr)
    end
end

function parse_id(expr :: Expr)
    # get parts
    parts = name_parts_from_expr(expr)

    # FQN = expr of parts seperated by "."
    function expr_from_fqname(name)
        if length(name) == 1
            return name[1]
        else
            return Expr(
                Symbol("."),
                expr_from_fqname(name[1:(length(name) - 1)]),
                QuoteNode(name[length(name)])
            )
        end
    end

    fqname = expr_from_fqname(parts)

    # "use" name: replace "." by _ in a symbol
    use_name = string(parts[1])
    for p in parts[2:length(parts)]
        use_name *= "_" * string(p)
    end
    use_name = Symbol(use_name)

    return identifier_t(use_name, parts, fqname)
end

# descriptive strings for types
function hash_type(t::continuous_t)
    return ("continuous_" * string(t.bit_width) * (t.complex ? "_1" : "_0"))
end

function hash_type(t::integer_t)
    return ("integer_" * string(t.bit_width) * (t.unsigned ? "_1" : "_0"))
end

function hash_type(t::void_t)
    return "void"
end

function hash_type(t::ptr_t)
    return ("ptr{" * hash_type(t.pointee_t) * "}")
end

function hash_type(t::struct_ref_t)
    return t.name.name
end

function hash_type(t::array_t)
    return ("array{" * hash_type(t.pointee_t) * "_" * string(t.dims) * "}")
end

function hash_type(t::tuple_t)
    str = "tuple{"
    for (ix, elem) in enumerate(t.members)
        if ix > 1
            str = str * "_"
        end
        str = str * hash_type(elem)
    end

    return (str * "}")
end

function hash_type(t::param)
    return hash_type(t.type)
end

function hash_type(t::struct_def)
    hash = t.name[0]
    for n in t.name[2:length(t.name)]
        hash *= "." * string(n)
    end

    return hash
end