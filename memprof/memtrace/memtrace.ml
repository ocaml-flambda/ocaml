type tracer = Memprof_tracer.t

let getpid64 () = Int64.of_int (Unix.getpid ())

let active_tracer : tracer option ref = ref None

let start_tracing ~context ~sampling_rate ~filename =
  if !active_tracer <> None then
    failwith "Only one Memtrace instance may be active at a time";
  let fd = Unix.openfile filename Unix.[O_CREAT;O_WRONLY;O_TRUNC] 0o600 in
  let info : Trace.Info.t =
    { sample_rate = sampling_rate;
      word_size = Sys.word_size;
      executable_name = Sys.executable_name;
      host_name = Unix.gethostname ();
      ocaml_runtime_params = Sys.runtime_parameters ();
      pid = getpid64 ();
      start_time = Trace.Timestamp.now ();
      context;
    } in
  let trace = Trace.Writer.create fd ~getpid:getpid64 info in
  let tracer = Memprof_tracer.start ~sampling_rate trace in
  active_tracer := Some tracer;
  tracer

let stop_tracing t =
  Memprof_tracer.stop t;
  active_tracer := None

let () =
  at_exit (fun () -> Option.iter stop_tracing !active_tracer)

let default_sampling_rate = 1e-6

let trace_if_requested ?context ?sampling_rate () =
  match Sys.getenv_opt "MEMTRACE" with
  | None | Some "" -> ()
  | Some filename ->
     (* Prevent spawned OCaml programs from being traced *)
     Unix.putenv "MEMTRACE" "";
     let sampling_rate_env =
       Option.bind (Sys.getenv_opt "MEMTRACE_RATE") float_of_string_opt in
     let sampling_rate =
       match sampling_rate, sampling_rate_env with
       | None, None -> default_sampling_rate
       | _, Some v -> v
       | Some v, None -> v in
     let _s = start_tracing ~context ~sampling_rate ~filename in
     ()

module Trace = Trace
module Memprof_tracer = Memprof_tracer
