type t =
  { mutable locked : bool;
    mutable failed : bool;
    mutable stopped : bool;
    report_exn : exn -> unit;
    trace : Trace.Writer.t }

let[@inline never] lock_tracer s =
  (* This is a maximally unfair spinlock. *)
  (* if s.locked then Printf.fprintf stderr "contention\n%!"; *)
  while s.locked do Thread.yield () done;
  if s.failed then
    false
  else
    (s.locked <- true; true)

let[@inline never] unlock_tracer s =
  assert (s.locked && not s.failed);
  s.locked <- false

let[@inline never] mark_failed s e =
  assert (s.locked && not s.failed);
  s.failed <- true;
  s.locked <- false;
  s.report_exn e

let default_report_exn e =
  let msg = Printf.sprintf "Memtrace failure: %s\n" (Printexc.to_string e) in
  output_string stderr msg;
  Printexc.print_backtrace stderr;
  flush stderr

let start ?(report_exn=default_report_exn) ~sampling_rate trace =
  let s = { trace; locked = false; stopped = false; failed = false; report_exn } in
  let minor_alloc_callback = (fun (info : Gc.Memprof.allocation) ->
      if lock_tracer s then begin
        match Trace.Writer.put_alloc_with_raw_backtrace trace (Trace.Timestamp.now ())
                ~length:info.size
                ~nsamples:info.n_samples
                ~is_major:false
                ~callstack:info.callstack
        with
        | r -> unlock_tracer s; Some r
        | exception e -> mark_failed s e; None
      end else None)
  in
  let major_alloc_callback = (fun (info : Gc.Memprof.allocation) ->
      if lock_tracer s then begin
        match Trace.Writer.put_alloc_with_raw_backtrace trace (Trace.Timestamp.now ())
                ~length:info.size
                ~nsamples:info.n_samples
                ~is_major:true
                ~callstack:info.callstack
        with
        | r -> unlock_tracer s; Some r
        | exception e -> mark_failed s e; None
      end else None)
  in
  let promote_callback = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_promote trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s; Some id
        | exception e -> mark_failed s e; None
      else None)
  in
  let minor_dealloc_callback = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_collect trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s
        | exception e -> mark_failed s e)
  in
  let major_dealloc_callback = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_collect trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s
        | exception e -> mark_failed s e)
  in
  Gc.Memprof.start
    ~sampling_rate
    ~callstack_size:max_int
    ~minor_alloc_callback
    ~major_alloc_callback
    ~promote_callback
    ~minor_dealloc_callback
    ~major_dealloc_callback
    ();
  s

let stop s =
  if not s.stopped then begin
    s.stopped <- true;
    Gc.Memprof.stop ();
    if lock_tracer s then
      Trace.Writer.close s.trace
  end
