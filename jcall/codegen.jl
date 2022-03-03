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

###
# General tools
###
primitive_cmap = Dict(
    :Nothing => Cvoid,
    :Number => Cdouble,
	:Complex => ComplexF64,
	:Real => Cdouble,
    :AbstractFloat => Cdouble,
    :BigFloat => Cdouble,
    :Float16 => Cfloat,
    :Float32 => Cfloat,
    :Float64 => Cdouble,
    :Integer => Clonglong,
    :BigInt => Clonglong,
    :Bool => Cuchar,
    :Signed => Clonglong,
    :Int128 => Clonglong,
    :Int16 => Cshort,
    :Int32 => Cint,
    :Int64 => Clonglong,
    :Int8 => Cchar,
    :Unsigned => Culonglong,
    :UInt128 => Culonglong,
    :UInt16 => Cushort,
    :UInt32 => Cuint,
    :UInt64 => Culonglong,
    :UInt8 => Cuchar
)

# creating unmanaged type (e.g. ptr + dims for arrays)
function create_unmanaged_type_replacements(t::param, reg :: type_definition_registry)
    t.managed = false

    if isa(t.type, struct_ref_t)
        t.managed = true

        # just replace by pointer
        t.replacement = param(
            t.name,
            ptr_t(t.type),
            false,
            nothing)
    end

    if isa(t.type, array_t)
        t.managed = true

        # replace by pointer to an equivalent struct
        t_sym = Symbol(hash_type(t.type))
        if !(t_sym in keys(reg.defs))
            reg.defs[t_sym] = t.type
            reg.names[t_sym] = Symbol("jl_implicit_" * string(length(keys(reg.defs))))
        end

        t_sym = reg.names[t_sym]

        tpl_struct = struct_ref_t(t_sym)
        
        t.replacement = param(
            t.name,
            ptr_t(tpl_struct),
            false,
            nothing)
    end

    if isa(t.type, tuple_t)
        t.managed = true

        # replace by pointer to an equivalent struct
        t_sym = Symbol(hash_type(t.type))
        if !(t_sym in keys(reg.defs))
            reg.defs[t_sym] = t.type
            reg.names[t_sym] = Symbol("jl_implicit_" * string(length(keys(reg.defs))))
        end

        t_sym = reg.names[t_sym]

        tpl_struct = struct_ref_t(t_sym)
        
        t.replacement = param(
            t.name,
            ptr_t(tpl_struct),
            false,
            nothing)
    end

end

function generate_implicit_structs(reg::type_definition_registry)

    for key in keys(reg.defs)
        stpl = reg.defs[key]
        members = []

        if isa(stpl, tuple_t)
            for ix in 1:length(stpl.members)
                push!(
                    members,
                    param(
                        Symbol("elem" * string(ix)),
                        stpl.members[ix],
                        false,
                        [])
                    )
            end
        elseif isa(stpl, array_t)
            # replace by pointer to native type and dimensions
            push!(
                members,
                param(
                    Symbol("ptr"),
                    ptr_t(stpl.pointee_t),
                    false, 
                    []
                ))

            for d in 1 : stpl.dims
                push!(
                    members, 
                    param(
                        Symbol("dim" * string(d)),
                        integer_t(64, true),
                        false,
                        []
                    ))
            end
        end

        push!(reg.adapted, (reg.names[key], members))
    end
end

###
# Julia
###

function deserialize(type_reg::type_definition_registry, val_type::primitive_t, replace_struct_t, replace_struct_ptr, val_expr)
    ins = [Expr(
        Symbol("="),
        val_expr,
        Expr(
        Symbol("call"),
        Symbol("unsafe_load"),
        Expr(
            Symbol("call"),
            Symbol("reinterpret"),
            expr_from_type(ptr_t(val_type)),
            replace_struct_ptr)))
    ]

    return ins
end

function deserialize(type_reg::type_definition_registry, val_type::ptr_t, replace_struct_t, replace_struct_ptr, val_expr)
    ins = [Expr(
        Symbol("="),
        val_expr,
        Expr(
        Symbol("call"),
        Symbol("unsafe_load"),
        Expr(
            Symbol("call"),
            Symbol("reinterpret"),
            expr_from_type(ptr_t(val_type)),
            replace_struct_ptr)))
    ]

    return ins
end

function deserialize(type_reg::type_definition_registry, val_type::array_t, replace_struct_t, replace_struct_ptr, val_expr)
    # array = structure with value pointer and one int per dimension
    ins = []
    
    # dimensions
    dim_expr = nothing

    dim_list = []
    for dim in 1:val_type.dims
        dim_sym = Symbol(string(val_expr) * "_dim" * string(dim))

        push!(
            ins,
            Expr(
                Symbol("="),
                dim_sym,
                generate_struct_field_read(
                    replace_struct_ptr,
                    replace_struct_t,
                    integer_t(64, false),
                    1 + dim
                )
            )
        )
        push!(dim_list, dim_sym)
    end

    if val_type.dims > 1
        dim_expr = Expr(
            Symbol("tuple"),
            dim_list...
        )
    else
        dim_expr = dim_list[1]
    end

    # create Array{...} from data -> stored in val_expr
    push!(
        ins, 
        Expr(
            Symbol("="),
            val_expr,
            Expr(
                Symbol("call"),
                Symbol("unsafe_wrap"),
                expr_from_type(val_type),
                generate_struct_field_read(
                    replace_struct_ptr,
                    replace_struct_t,
                    ptr_t(val_type.pointee_t),
                    1
                ),
                dim_expr
            )
        )
    )

    # push!(
    #     ins, 
    #     Expr(
    #         Symbol("="),
    #         val_expr,
    #         Expr(
    #             Symbol("call"),
    #             Symbol("unsafe_wrap"),
    #             expr_from_type(val_type),
    #             Expr(
    #                 Symbol("call"),
    #                 Symbol("unsafe_load"),
    #                 struct_member_ptr(
    #                     replace_struct_ptr,
    #                     replace_struct_t,
    #                     ptr_t(val_type.pointee_t),
    #                     1
    #                 )
    #             ),
    #             dim_expr
    #         )
    #     )
    # )

    return ins
end

function deserialize(type_reg::type_definition_registry, val_type::struct_ref_t, replace_struct_t, replace_struct_ptr, val_expr)
    ins = []
    ctr = 0 

    # retrieve struct definition (note: struct_ref_t as original type always refers
    # to explicit structs -> hence, there is no replacement)
    # note that once struct members have been deserialized, we need to instantiate the 
    # original structure, not the replacement that we have created
    hash_name = val_type.name.name
    val_type_def = type_reg.defs[val_type.name.name]

    # println("Deserialize struct: ", hash_name, " with replacement type ", replace_struct_t)

    # deserialize each individual member
    member_vars = []
    for (ix, member) in enumerate(val_type_def.members)
        member_var = Symbol(string(val_expr) * "_" * string(ctr))

        # println(ix, " -> ", member)

        # compute offset to this member
        member_ptr = nothing
        if isa(member.type, primitive_t)
            member_ptr = struct_member_ptr(
                replace_struct_ptr, 
                replace_struct_t, 
                member.type,
                ix)
        else
            member_ptr = generate_struct_field_read(
                replace_struct_ptr, 
                replace_struct_t, 
                ptr_t(member.replacement),
                ix)
        end
        
        append!(
            ins,
            deserialize(
                type_reg,
                member.type,
                isa(member.type, primitive_t) ? nothing : member.replacement,
                member_ptr,
                member_var
            )
        )

        push!(member_vars, member_var)

        ctr += 1
    end

    # now deserialize the _actual_ structure
    push!(
        ins,
        Expr(
            Symbol("="),
            val_expr,
            Expr(
                Symbol("call"),
                val_type_def.name.fqname,
                member_vars...
            )
        )
    )

    return ins
end

function deserialize(type_reg::type_definition_registry, val_type::tuple_t, replace_struct_t, replace_struct_ptr, val_expr)
    ins = []
    ctr = 0 

    # retrieve struct definition
    val_type_def = type_reg.defs[replace_struct_t.name.name]

    # println("Deserialize tuple: ", val_type, " with replacement type ", replace_struct_t)

    # deserialize each individual member
    member_vars = []
    for (ix, member) in enumerate(val_type_def.members)
        member_var = Symbol(string(val_expr) * "_" * string(ctr))

        # println("-> ", member)

        # compute offset to this member
        member_ptr = nothing
        if isa(member.type, primitive_t)
            member_ptr = struct_member_ptr(
                replace_struct_ptr, 
                replace_struct_t, 
                member.type,
                ix,
                true)
        else
            member_ptr = generate_struct_field_read(
                replace_struct_ptr, 
                replace_struct_t, 
                ptr_t(member.replacement),
                ix)
        end
        
        append!(
            ins,
            deserialize(
                type_reg,
                member.type,
                isa(member.type, primitive_t) ? nothing : member.replacement,
                member_ptr,
                member_var
            )
        )

        push!(member_vars, member_var)

        ctr += 1
    end

    # now deserialize the _actual_ structure
    push!(
        ins,
        Expr(
            Symbol("="),
            val_expr,
            Expr(
                Symbol("tuple"),
                member_vars...
            )
        )
    )

    return ins 
end

function serialize!(ptr_sym, ret_sym, ret_type::array_t, repl_struct_ref, type_defs::type_definition_registry)
    ins = []

    # alloc pointer
    push!(
        ins,
        Expr(
            Symbol("="),
            ptr_sym,
            Expr(
                Symbol("call"),
                Expr(
                    Symbol("."),
                    Symbol("Libc"),
                    QuoteNode(Symbol("malloc"))
                ),
                Expr(
                    Symbol("call"),
                    Symbol("sizeof"),
                    repl_struct_ref.name.name
                )
            )
        )
    )
    push!(
        ins,
        Expr(
            Symbol("="),
            ptr_sym,
            Expr(
                Symbol("call"),
                Symbol("reinterpret"),
                Expr(
                    Symbol("curly"),
                    Symbol("Ptr"),
                    repl_struct_ref.name.name
                ),
                ptr_sym
            )
        )
    )
    
    # compute number of elements
    dim_exprs = []
    for dim in 1:ret_type.dims
        push!(
            dim_exprs,
            Expr(
                Symbol("call"),
                Symbol("size"),
                ret_sym,
                dim
            )
        )
    end

    dim_expr = Expr(
        Symbol("call"),
        Symbol("size"),
        ret_sym,
        1
    )

    for dim in 2:ret_type.dims
        dim_expr = Expr(
            Symbol("call"),
            Symbol("*"),
            dim_expr,
            Expr(
                Symbol("call"),
                Symbol("size"),
                ret_sym,
                dim
            )       
        )
    end

    # write data to storage (after allocating memory for it)
    val_ptr_sym = Symbol(String(ptr_sym) * "_arr")
    push!(
        ins,
        Expr(
            Symbol("="),
            val_ptr_sym,
            Expr(
                Symbol("call"),
                Expr(
                    Symbol("."),
                    Symbol("Libc"),
                    QuoteNode(Symbol("malloc"))
                ),
                Expr(
                    Symbol("call"),
                    Symbol("*"),
                    dim_expr,
                    Expr(
                        Symbol("call"),
                        Symbol("sizeof"),
                        expr_from_type(ret_type.pointee_t)
                    )
                )
            )
        )
    )
    push!(
        ins,
        Expr(
            Symbol("="),
            val_ptr_sym,
            Expr(
                Symbol("call"),
                Symbol("reinterpret"),
                Expr(
                    Symbol("curly"),
                    Symbol("Ptr"),
                    expr_from_type(ret_type.pointee_t)
                ),
                val_ptr_sym
            )
        )
    )

    push!(
        ins,
        Expr(
            Symbol("call"),
            Symbol("unsafe_copyto!"),
            val_ptr_sym,
            Expr(
                Symbol("call"),
                Symbol("pointer"),
                ret_sym
            ),
            dim_expr
        )
    )

    # create return struct,, then serialize it
    ret_val = Symbol(String(ptr_sym) * "_val")
    push!(
        ins,
        Expr(
            Symbol("="),
            ret_val,
            Expr(
                Symbol("call"),
                repl_struct_ref.name.name,
                val_ptr_sym,
                dim_exprs...
            )
        )
    )
    push!(
        ins,
        Expr(
            Symbol("call"),
            Symbol("unsafe_store!"),
            ptr_sym,
            ret_val
        )
    )

    return ins
end

function serialize!(ptr_sym, ret_sym, ret_type::struct_ref_t, repl_struct_ref, type_defs::type_definition_registry)
    ins = []

    # alloc pointer
    push!(
        ins,
        Expr(
            Symbol("="),
            ptr_sym,
            Expr(
                Symbol("call"),
                Expr(
                    Symbol("."),
                    Symbol("Libc"),
                    QuoteNode(Symbol("malloc"))
                ),
                Expr(
                    Symbol("call"),
                    Symbol("sizeof"),
                    repl_struct_ref.name.name
                )
            )
        )
    )
    push!(
        ins,
        Expr(
            Symbol("="),
            ptr_sym,
            Expr(
                Symbol("call"),
                Symbol("reinterpret"),
                Expr(
                    Symbol("curly"),
                    Symbol("Ptr"),
                    repl_struct_ref.name.name
                ),
                ptr_sym
            )
        )
    )

    # serialize members
    member_exprs = []
    
    val_type_def = type_defs.defs[ret_type.name.name]

    for (ix, member) in enumerate(val_type_def.members)
        acc_expr = Expr(
            Symbol("."),
            ret_sym,
            QuoteNode(member.name)
        )

        if isa(member.type, primitive_t)
            # primitive value -> just copy
            push!(member_exprs, acc_expr)
        else
            # complex type -> deserialize to additional pointer
            member_sym = Symbol(String(ptr_sym) * "_" * string(ix))
            append!(
                ins,
                serialize!(member_sym, acc_expr, member.type, member.replacement, type_defs)
            )
            push!(member_exprs, member_sym)
        end
    end

    # create return struct,, then serialize it
    ret_val = Symbol(String(ptr_sym) * "_val")
    push!(
        ins,
        Expr(
            Symbol("="),
            ret_val,
            Expr(
                Symbol("call"),
                repl_struct_ref.name.name,
                member_exprs...
            )
        )
    )
    push!(
        ins,
        Expr(
            Symbol("call"),
            Symbol("unsafe_store!"),
            ptr_sym,
            ret_val
        )
    )

end

function serialize!(ptr_sym, ret_sym, ret_type::tuple_t, repl_struct_ref, type_defs::type_definition_registry)
    ins = []

    # alloc pointer
    push!(
        ins,
        Expr(
            Symbol("="),
            ptr_sym,
            Expr(
                Symbol("call"),
                Expr(
                    Symbol("."),
                    Symbol("Libc"),
                    QuoteNode(Symbol("malloc"))
                ),
                Expr(
                    Symbol("call"),
                    Symbol("sizeof"),
                    repl_struct_ref.name.name
                )
            )
        )
    )
    push!(
        ins,
        Expr(
            Symbol("="),
            ptr_sym,
            Expr(
                Symbol("call"),
                Symbol("reinterpret"),
                Expr(
                    Symbol("curly"),
                    Symbol("Ptr"),
                    repl_struct_ref.name.name
                ),
                ptr_sym
            )
        )
    )

    # serialize members
    member_exprs = []
    
    val_type_def = type_defs.defs[repl_struct_ref.name.name]

    for (ix, member) in enumerate(val_type_def.members)
        acc_expr = Expr(
            Symbol("ref"),
            ret_sym,
            ix
        )

        if isa(member.type, primitive_t)
            # primitive value -> just copy
            push!(member_exprs, acc_expr)
        else
            # complex type -> deserialize to additional pointer
            member_sym = Symbol(String(ptr_sym) * "_" * string(ix))
            append!(
                ins,
                serialize!(member_sym, acc_expr, member.type, member.replacement, type_defs)
            )
            push!(member_exprs, member_sym)
        end
    end

    # create return struct,, then serialize it
    ret_val = Symbol(String(ptr_sym) * "_val")
    push!(
        ins,
        Expr(
            Symbol("="),
            ret_val,
            Expr(
                Symbol("call"),
                repl_struct_ref.name.name,
                member_exprs...
            )
        )
    )
    push!(
        ins,
        Expr(
            Symbol("call"),
            Symbol("unsafe_store!"),
            ptr_sym,
            ret_val
        )
    )

end

function generate_julia_struct_def(name, members)
    # create list of members
    expr_members = []

    for member in members
        push!(
            expr_members,
            Expr(
                Symbol("::"),
                member.name,
                expr_from_type(member.managed ? ptr_t(member.replacement) : member.type)
            )
        )
    end

    return [
        Expr(
            Symbol("struct"),
            true,
            name,
            Expr(
                Symbol("block"),
                expr_members...
            )
        ),
        Expr(
            Symbol("export"),
            Symbol(name))
    ]
end

function generate_struct_field_write(struct_ptr, struct_ref_type, elem_type, member_ix, rhs_expr)
    return Expr(
        Symbol("call"),
        Symbol("unsafe_store!"),
        Expr(
            Symbol("call"),
            Symbol("reinterpret"),
            expr_from_type(ptr_t(elem_type)),
            Expr(
                Symbol("call"),
                Symbol("+"),
                struct_ptr,
                Expr(
                    Symbol("call"),
                    Symbol("fieldoffset"),
                    expr_from_type(struct_ref_type),
                    member_ix)
            )
        ),
        rhs_expr
    )
end

function struct_member_ptr(struct_ptr, struct_ref_type, elem_type, member_ix, raw = false)
    raw_ptr = Expr(
        Symbol("call"),
        Symbol("+"),
        Expr(
            Symbol("call"),
            Symbol("reinterpret"),
            expr_from_type(ptr_t(integer_t(8, false))),
            struct_ptr
        ),
        Expr(
            Symbol("call"),
            Symbol("fieldoffset"),
            expr_from_type(struct_ref_type),
            member_ix)
    )

    if raw
        return raw_ptr
    end

    return Expr(
        Symbol("call"),
        Symbol("reinterpret"),
        expr_from_type(ptr_t(elem_type)),
        raw_ptr
    )
end

function generate_struct_field_read(struct_ptr, struct_ref_type, elem_type, member_ix)
    return Expr(
        Symbol("call"),
        Symbol("unsafe_load"),
        struct_member_ptr(struct_ptr, struct_ref_type, elem_type, member_ix)
    )
end

function generate_julia_wrapper_function(type_reg::type_definition_registry, name, call_sym, ret, params)
    fun_name = Symbol("jl_" * string(name))

    # function name (plus prefix)
    call_args = []
    push!(call_args, fun_name)

    # arguments -> use pointers for structs
    for param in params
        # println(param)

        push!(
            call_args,
            Expr(
                Symbol("::"),
                param.name,
                expr_from_type(param.replacement === nothing ? param.type : ptr_t(param.replacement))
            )
        )
    end

    signature = Expr(
        Symbol("::"),
        Expr(
            :call,
            call_args...
        ),
        expr_from_type(ret.replacement === nothing ? ret.type : ptr_t(ret.replacement))
    )

    ##
    # deserialization
    ##
    deserialized_args = []
    ins = []

    for (ctr, param) in enumerate(params)
        # println("PARAM ", param.name, " ", param.type, " ", isa(param.type, ptr_t) ? param.type.pointee_t : "")

        if isa(param.type, primitive_t)
            push!(
                deserialized_args,
                param.name)
        else
            param_var = Symbol("var_" * string(ctr))

            append!(
                ins,
                deserialize(
                    type_reg,
                    param.type,
                    param.replacement,
                    param.name,
                    param_var
                )
            )
            push!(
                deserialized_args,
                param_var
            )
        end
    end

    ##
    # call actual function
    ##
    call_expr = Expr(
        Symbol("call"),
        # Symbol(name),
        call_sym,
        deserialized_args...
    )

    ret_val_sym = Symbol("ret_val")
    if typeof(ret.type) != void_t
        if isa(ret.type, primitive_t)
            call_expr = Expr(
                Symbol("return"),
                call_expr
            )
        else
            call_expr = Expr(
                Symbol("="),
                ret_val_sym,
                call_expr
            )
        end
    end

    push!(
        ins,
        call_expr
    )

    ##
    # serialization of return values
    ##
    wrap_in_return = false
    if typeof(ret.type) != void_t
        if !isa(ret.type, primitive_t)
            ret_ptr_sym = Symbol("ret_ptr")
            append!(
                ins,
                serialize!(ret_ptr_sym, ret_val_sym, ret.type, ret.replacement, 
                    type_reg)
            )

            # complex return type, needs to be serialized to 
            # memory that is allocated by Julia's Libc-Wrapper

            # return the pointer to C
            push!(
                ins,
                Expr(
                    Symbol("return"),
                    ret_ptr_sym
                ))
        end
    end

    ## 
    # macrocall structure
    ##
    return [
        Expr(
            :macrocall,
            Expr(
                Symbol("."),
                :Base, 
                QuoteNode(Symbol("@ccallable"))
            ),
            Nothing,
            Expr(
                :function,
                signature, 
                Expr(
                    :block,
                    ins...
                )
            )
        ),
        Expr(
            Symbol("export"),
            fun_name
        )
    ]
end

###
# C
###

function generate_c_struct_def(name, members)
    cstr = "typedef struct {\n"

    # generate a list of unmanaged members
    unm_members = []
    for p in members
        if p.managed
            push!(unm_members, param(p.name, ptr_t(p.replacement), false, nothing))
        elseif typeof(p.type) == struct_ref_t
            push!(unm_members, param(p.name, ptr_t(p.type), false, nothing))
        else
            push!(unm_members, p)
        end
    end

    for member in unm_members
        cstr *= "\t" * cstring_from_type(member.type) * " " * string(member.name) * ";\n"
    end
    cstr *= "} " * string(name) * ";\n"

    return cstr
end

function generate_c_function_def(name, rettype, params)
    cstr = cstring_from_type(rettype.replacement === nothing ? rettype.type : 
        ptr_t(rettype.replacement))
    cstr *= " jl_" * string(name) * "(\n"

    # generate a list of unmanaged members
    unm_params = []
    for p in params
        if p.managed
            push!(unm_params, param(p.name, ptr_t(p.replacement), false, nothing))
        elseif typeof(p.type) == struct_ref_t
            push!(unm_params, param(p.name, ptr_t(p.type), false, nothing))
        else
            push!(unm_params, p)
        end
    end

    for (ix, p) in enumerate(unm_params)

        cstr *= "\t" * cstring_from_type(p.type) * " " * string(p.name)
        if ix < length(unm_params)
            cstr *= ",\n"
        end
    end
    cstr *= ");"

    return cstr
end