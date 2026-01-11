use std::{fs::File, io::Write, mem};

use cranelift::{codegen::{Context, control::ControlPlane, ir::{Function, UserFuncName}, verify_function}, object::ObjectBuilder, prelude::{isa::{OwnedTargetIsa, lookup_by_name}, settings::Flags}};
use cranelift::prelude::*;
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{Linkage, Module, default_libcall_names};
use cranelift::object::ObjectModule;

fn oke() {
    let flags = settings::Flags::new(settings::builder());

    let isa_builder = cranelift::native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {msg}");
    });
    let isa = isa_builder
        .finish(flags.clone())
        .unwrap();

    // let mut module = JITModule::new(JITBuilder::with_isa(isa.clone(), default_libcall_names()));
    let mut module = ObjectModule::new(ObjectBuilder::new(isa.clone(), "simple", default_libcall_names()).unwrap());
    add_add_to_module(&mut module, isa, &flags);

    // // if jit:
    // module.finalize_definitions().unwrap();
    // let code_b = module.get_finalized_function(func);
    // let ptr_b = unsafe { mem::transmute::<_, extern "C" fn(a: i64, b: i64) -> i64>(code_b) };
    // let res = ptr_b(3, 18);
    // dbg!(res);

    // if object:

    let declarations = module.declarations();
    let mut header_file = String::new();
    header_file.push_str("#include <stdint.h>\n");
    for (fn_id, func) in declarations.get_functions() {
        dbg!(fn_id, func);
        if func.linkage == Linkage::Export {
            match func.signature.returns.len() {
                0 => header_file.push_str("void"),
                1 => {
                    header_file.push_str(&types_to_c_type(func.signature.returns[0].value_type))
                }
                _ => {
                    panic!("multiple return values")
                }
            }
            header_file.push(' ');
            header_file.push_str(func.name.as_ref().unwrap());
            header_file.push('(');
            let param_length = func.signature.params.len();
            for (i, param) in func.signature.params.iter().enumerate() {
                    header_file.push_str(&types_to_c_type(param.value_type));
                    header_file.push(' ');
                    header_file.push_str(&format!("arg{i}"));
                    if i+1 != param_length {
                        header_file.push(',');
                        header_file.push(' ');
                    }
            }
            header_file.push(')');
            header_file.push(';');
        }
    }
    std::fs::write("out/hallo.h", header_file).unwrap();
    


    let finished_object = module.finish();
    let bytes = finished_object.emit().unwrap();
    std::fs::write("out/hallo.obj", bytes).unwrap();

    // let mut ctx = Context::for_function(func);
    // let isa_builder = lookup_by_name("x86_64").unwrap();
    // let isa = isa_builder.finish(flags).unwrap();
    // let mut control_plane = ControlPlane::default();
    // let compiled_code = ctx.compile(isa.as_ref(), &mut control_plane).unwrap();
    // dbg!(compiled_code);


}

fn types_to_c_type(type_: Type) -> &'static str {
    match type_ {
        types::I8 => "int8_t",
        types::I32 => "int32_t",
        types::I64 => "int64_t",
        _ => unimplemented!()
    }
}

fn add_add_to_module(module: &mut impl Module, isa: OwnedTargetIsa, flags: &Flags) {

    let mut ctx = module.make_context();

    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));

    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    // let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);

    // let func = module.declare_function("add", Linkage::Local, &sig).unwrap(); // for jit the function doesnt need to be externa;
    let func = module.declare_function("add", Linkage::Export, &sig).unwrap(); // for object it does

    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, func.as_u32());

    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let block0 = builder.create_block();
        let x = builder.declare_var(types::I64);
        let y = builder.declare_var(types::I64);
        let z = builder.declare_var(types::I64);
        builder.append_block_params_for_function_params(block0);

        builder.switch_to_block(block0);
        builder.seal_block(block0);
        {
            let tmp = builder.block_params(block0)[0];
            builder.def_var(x, tmp);
        }
        {
            let tmp = builder.block_params(block0)[1];
            builder.def_var(y, tmp);
        }
        {
            let arg1 = builder.use_var(x);
            let arg2 = builder.use_var(y);
            let tmp = builder.ins().iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(z);
            builder.ins().return_(&[arg]);
        }

         builder.finalize();
    }
    let res = verify_function(&ctx.func, flags);
    // println!("{}", ctx.func.display());
    if let Err(errors) = res {
        panic!("{}", errors);
    }

    module.define_function(func, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
}




#[test]
fn testingintestg() {
    oke();

    panic!()
}