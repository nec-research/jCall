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

include("typestructs.jl")

# register composite types, implicit and explicit
mutable struct type_definition_registry
    defs :: Dict
    hash_name_map
    found_order
    anonymous_counter

    function type_definition_registry()
        new(Dict(), Dict(), [], 1)
    end
end

function import_struct_def(type_reg :: type_definition_registry, id::identifier_t)

    # create type from name
    name_type = eval(id.fqname)

    # create code for struct definition
    member_expr = []
    for (fname, ftype) in zip(fieldnames(name_type), name_type.types)
        ptype_expr = Meta.parse(string(ftype))
        push!(
            member_expr,
            Expr(
                Symbol("::"),
                fname,
                ptype_expr
            )
        )
    end

    struct_expr = Expr(
        Symbol("struct"),
        true,
        id.fqname,
        Expr(
            Symbol("block"),
            member_expr...
        )
    )

    # dump(struct_expr)
    p_struct = param_from_expr!(type_reg, struct_expr, id)
end

function prepend_id!(id_s)
    # only modify short name
    id_s.name = Symbol("jl_" * String(id_s.name))
end

# parsing from expr
function param_from_expr!(type_reg :: type_definition_registry, expr, p_id)

    # parse type
    p_name = nothing
    p_type = nothing
    p_managed = false
    p_composite = false

    if isa(expr, Symbol)
        sym_string = string(expr)

        if expr in supported_continuous_type_symbols

            # primitive type
            p_type = continuous_t(
                parse(Int, sym_string[6:length(sym_string)]),
                false
            )            

        elseif expr in supported_int_type_symbols
            # primitive type

            if sym_string == "Bool"
                p_type = integer_t(1, true)
            elseif occursin("UInt", sym_string)
                p_type = integer_t(parse(Int, sym_string[5:length(sym_string)]), true)
            else
                p_type = integer_t(parse(Int, sym_string[4:length(sym_string)]), false)
            end

        elseif expr in [:Any, :Nothing]
            p_type = void_t()

        elseif expr in keys(type_reg.hash_name_map)
            # must be a reference to a custom (but explicit) struct whose
            # definition we must already have encountered
            p_managed = false
            p_type = struct_ref_t(type_reg.hash_name_map[expr])
        end
    else
        # not a symbol, so must be an expression (pointer or array / vector)
        if expr.head == Symbol("curly")

            if expr.args[1] == Symbol("Ptr")
                pointee_param = param_from_expr!(type_reg, expr.args[2], nothing)

                p_type = pointer_t(pointee_param.type)

            elseif expr.args[1] == Symbol("Vector")
                scalar_param = param_from_expr!(type_reg, expr.args[2], nothing)

                p_type = array_t(scalar_param.type, 1)
                p_composite = true
                p_managed = true

            elseif expr.args[1] == Symbol("Matrix")
                scalar_param = param_from_expr!(type_reg, expr.args[2], nothing)

                p_type = array_t(scalar_param.type, 2)
                p_composite = true
                p_managed = true

            elseif expr.args[1] == Symbol("Array")
                scalar_param = param_from_expr!(type_reg, expr.args[2], nothing)
                num_dims = expr.args[3]

                p_type = array_t(scalar_param.type, num_dims)
                p_composite = true
                p_managed = true

            elseif expr.args[1] == Symbol("Tuple")
                members = []

                for (ix, m_expr) in enumerate(expr.args)
                    if ix > 1
                        m_param = param_from_expr!(type_reg, m_expr, nothing)
                        push!(members, m_param)
                    end
                end

                p_type = tuple_t(members)
                p_composite = true
                p_managed = true

            end

        elseif expr.head == Symbol(".")

            # seems to be a name of some sorts -> without any other type,
            # must be a custom struct

            p_managed = false
            struct_id = parse_id(expr)

            if !(struct_id.name in keys(type_reg.hash_name_map))
                # need to retrieve this struct from the runtime
                import_struct_def(type_reg, struct_id)
            end

            p_id = type_reg.defs[type_reg.hash_name_map[struct_id.name]].name
            p_type = struct_ref_t(p_id)

        elseif expr.head == Symbol("::")
            # named member of struct
            p_name = expr.args[1]
            named_param = param_from_expr!(type_reg, expr.args[2], nothing)
            p_type = named_param.type
            p_managed = named_param.managed

        elseif expr.head == Symbol("block")
            # struct definition
            members = []

            for (ix, m_expr) in enumerate(expr.args)
                m_param = param_from_expr!(type_reg, m_expr, nothing)
                push!(members, m_param)
            end

            p_type = struct_def(p_id, members)
            p_composite = true
            p_managed = true

        elseif expr.head == Symbol("struct")
            # reference to struct
            struct_id = parse_id(expr.args[2])
            struct_name = struct_id.name

            if !(struct_name in keys(type_reg.hash_name_map))
                pparam = param_from_expr!(type_reg, expr.args[3], struct_id)
            end

            p_managed = false
            p_id = type_reg.defs[type_reg.hash_name_map[struct_name]].name
            p_type = struct_ref_t(p_id)
        end
    end

    if p_type !== nothing
        if p_composite
            hash_name = Symbol((p_id === nothing) ? hash_type(p_type) : p_id.name)

            if !(hash_name in keys(type_reg.hash_name_map))
                alias_name = hash_name
                if p_id === nothing
                    alias_name = Symbol("jl_implicit" * string(type_reg.anonymous_counter))
                    type_reg.anonymous_counter += 1
                else
                    # custom struct -> prepend "jl_"
                    alias_name = Symbol("jl_" * string(alias_name))
                    
                    prepend_id!(p_id)
                end

                # register this structure
                # println("NEW: ", hash_name, " -> ", alias_name, " -> ", p_id)

                type_reg.defs[alias_name] = p_type
                type_reg.hash_name_map[hash_name] = alias_name
                push!(type_reg.found_order, alias_name)

            end
        end

        return param(p_name, p_type, p_managed, nothing)
    end
    
    error("Unsupported type:", expr)
end

function create_composite_struct_def!(type_reg :: type_definition_registry, composite_tpl)
    composite_param = param_from_expr!(type_reg, composite_tpl[2], 
        (composite_tpl[1] !== nothing) ? composite_tpl[1] : nothing)

    return composite_param
end

function embed_replacements!(type_reg :: type_definition_registry)
    for key in type_reg.found_order
        def = type_reg.defs[key]

        # println(def)

        # process membersc
        if typeof(def) in [struct_def, tuple_t]
            for ix in 1:length(def.members)
                if def.members[ix].managed
                    hash_name = Symbol(hash_type(def.members[ix]))
                    new_param_name = type_reg.hash_name_map[hash_name]

                    def.members[ix].managed = true
                    def.members[ix].replacement = struct_ref_t(
                        identifier_t(new_param_name, [new_param_name], new_param_name))

                elseif isa(def.members[ix].type, struct_ref_t)
                    def.members[ix].replacement = def.members[ix].type
                end
            end
        end

        # generate new structure definitions for implicit types (replace
        # by structure defs)
        if typeof(def) == array_t
            replacement = struct_def(
                identifier_t(key, [key], key), 
                [
                    param(
                        Symbol("vals"),
                        ptr_t(def.pointee_t),
                        false,
                        nothing)
                ])

            for ix in 1:def.dims
                push!(
                    replacement.members,
                    param(
                        Symbol("dim" * string(ix)),
                        integer_t(64, true),
                        false,
                        nothing
                    ))
            end

            type_reg.defs[key] = replacement

        elseif typeof(def) == tuple_t
            replacement = struct_def(
                identifier_t(key, [key], key), 
                [])
            
            for (ix, member) in enumerate(def.members)
                push!(
                    replacement.members,
                    param(
                        Symbol("elem" * string(ix)),
                        member.type,
                        member.managed,
                        member.replacement
                    )
                )
            end

            type_reg.defs[key] = replacement
        end
    end
end

function replace_function_param!(fun_param, type_reg :: type_definition_registry)

    # use a pointer for composite types
    if isa(fun_param.type, composite_t)
        composite_hash = Symbol(hash_type(fun_param.type))

        if composite_hash in keys(type_reg.hash_name_map)
            hname = type_reg.hash_name_map[composite_hash]
            fun_param.replacement = struct_ref_t(
                identifier_t(hname, [hname], hname))

        elseif composite_hash in keys(type_reg.defs)
            # covers user-define structs
            fun_param.replacement = fun_param.type
        end
    end
end