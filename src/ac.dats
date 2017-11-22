// A faster version of 'cat'.

staload UNSAFE = "prelude/SATS/unsafe.sats"

staload FCNTL = "libc/SATS/fcntl.sats"
stadef fildes_v = $FCNTL.fildes_v
macdef RDONLY = $FCNTL.O_RDONLY

staload UNISTD = "libc/SATS/unistd.sats"
macdef STDIN_FILENO = $UNISTD.STDIN_FILENO
macdef STDOUT_FILENO = $UNISTD.STDOUT_FILENO

extern
fun{env:viewt@ype} getchars
  {n:nat}
  (env: &env, buf: &bytes(n), n: size_t (n)) : sizeLte (n)

extern
fun{env:viewt@ype}
  putchars {n:nat} {n1:nat | n1 <= n}
    (env: &env, buf: &bytes(n), n1: size_t n1): void

// catloop takes a buffer, its size, and two proofs as arguments. The proofs
// guarantee that values of type 'env1' and 'env2' exist at the appropriate
// locations and that we can peek at the value safely.
fun{env1,env2:viewt@ype} catloop
  {n:nat}
  (env1: &env1, env2: &env2, buf: &bytes(n), n: size_t n) : void = 
    let
      val n1 = getchars<env1> (env1, buf, n)
    in
      if n1 > 0 then
        ( putchars<env2> (env2, buf, n1); 
          catloop (env1, env2, buf, n) )
      else ()
    end

%{^
typedef struct {
  int strip_ansi ;
} params_t ;
%}
typedef params =
  // a data type for our command-line options
  $extype_struct
    "params_t" of 
      { strip_ansi = bool // strip ansi escape codes
      }

extern
fun params_copy (to: &params, from: &params) : void = "mycat_params_copy"

implement params_copy (to, from) = 
  {
    val () = to.strip_ansi := from.strip_ansi
  }

fn quote_output(params: &params): bool =
  params.strip_ansi

%{^
typedef
struct {
  int fildes ;
} envinp_t ;
%}
viewtypedef
envinp (fd:int) =
  $extype_struct "envinp_t" of
    { fildes= int (fd)        // file descriptor
    , fildes_v= fildes_v (fd) // file descriptor view
    }

%{^
typedef
struct {
  int dummy ;
} envstdout_t ;
%}
viewtypedef
envstdout () =
  $extype_struct "envstdout_t" of
    { dummy= int
    }

local
  sta fd: int
in
  implement getchars<envinp(fd)> (env, buf, n) =
    let
      // the number of characters read
      val n1 = $FCNTL.read_exn (env.fildes_v | env.fildes, buf, n)
    in
      n1
    end
end

implement
  putchars<envstdout()> (env, buf, n1) =
    let
      val (pf_stdout | ()) = $UNISTD.stdout_fildes_view_get ()
      val n1 = $FCNTL.write_all_err (pf_stdout | STDOUT_FILENO, buf, n1)
      val () = $UNISTD.stdout_fildes_view_set (pf_stdout|)
    in end

// just dump the output (buffered of course)
fun readout_raw {fd:int} 
  (pf: !fildes_v fd | fd: int fd) : void =
    let
      var env1: envinp(fd)
      val () = env1.fildes := fd
      // consume file descriptor
      prval () = env1.fildes_v := pf
      var env2: envstdout ()
      val () = env2.dummy := 0
      #define BUFSZ 4096
      var !p_buf with pf_buf = @[byte][BUFSZ]()
      prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
      val () = catloop<envinp(fd),envstdout()> (env1, env2, !p_buf, BUFSZ)
      prval () = pf := env1.fildes_v
    in end

absview cbuf_v (l0:addr, n: int, l:addr)

%{^
  ATSinline()
  ats_void_type
  cbuf_putchar (ats_ptr_type p, ats_char_type c)
  {
    *(char*)p = c ; return ;
  }

  ATSinline()
  ats_void_type
  cbuf_clearall (ats_ptr_type p0, ats_ptr_type p) 
  {
    if (p0 < p) {
      atslib_fildes_write_all_exn (STDOUT_FILENO, p0, (char*)p - (char*)p0) ;
    }
    return ;
  }
%}

extern
fun cbuf_putchar
  {n:nat}
  {l0:addr}
  {l:addr | l < l0 + n}
  (pf: !cbuf_v (l0, n, l) >> cbuf_v (l0, n, l+1) | p: ptr l, c: char) : void =
    "mac#cbuf_putchar"

extern
  fun cbuf_clearall
    {n:nat}
    {l0:addr}
    {l:addr | l <= l0 + n}
    (pf: !cbuf_v (l0, n, l) >> cbuf_v (l0, n, l0) | p0: ptr l0, p: ptr l) : void =
      "mac#cbuf_clearall"

fun putchar_stripped_buf
  {n:nat}
  {l0:addr}
  {l:addr | l + 1 <= l0 + n}
  ( pfbuf: !cbuf_v (l0, n, l) >> cbuf_v (l0, n, l) | params: &params
  , b: char, p0: ptr l0, p: ptr l ) : #[l:addr | l <= l0+n] ptr l =
    let
      macdef putc (p, c) = cbuf_putchar (pfbuf | ,(p), ,(c))
    in
      ( putc (p, b); p+1 )
    end

%{^
#define CBUFSZ 8192
%}
#define CBUFSZ 8192

// helper for parser
fun is_digit(c: char) : bool =
  case c of
    | '0' => true
    | '1' => true
    | '2' => true
    | '3' => true
    | '4' => true
    | '5' => true
    | '6' => true
    | '7' => true
    | '8' => true
    | '9' => true
    | ';' => true
    | _ => false

// helper for parser
fun skip_char(b: char) : bool =
  let
    val ch = int_of_uchar ((uchar_of_char)b)
  in
    case+ 0 of
      | _ when ch < 32 => begin
        case b of
          | '\t' => false
          | '\n' => false
          | _ => true
        end
      | _ => false
  end

fun putchars_stripped
  {n:int}
  {n1,i:nat | i <= n1; n1 <= n}
  {l0:addr} {l:addr | l <= l0+CBUFSZ}
  ( pfbuf: !cbuf_v (l0, CBUFSZ, l) >> cbuf_v (l0, CBUFSZ, l0) | params: &params, skip_count: int
  , cs: &bytes(n), n1: size_t n1, i: size_t i, p0: ptr l0, p: ptr l ) : void =
    if i < n1 then
      let
        val b = (char_of_byte)cs.[i]
      in
        if not(skip_char(b)) && skip_count = 0 then
          if p + 4 <= p0 + CBUFSZ then
            let
              val p = putchar_stripped_buf (pfbuf | params, b, p0, p)
            in
              putchars_stripped (pfbuf | params, 0, cs, n1, i+1, p0, p)
          end
          else
            let
              val () = cbuf_clearall (pfbuf | p0, p)
              val p = putchar_stripped_buf (pfbuf | params, b, p0, p0)
            in
              putchars_stripped (pfbuf | params, 0, cs, n1, i+1, p0, p)
          end
        else if not(skip_char(b)) && not(is_digit(b)) then
          putchars_stripped (pfbuf | params, skip_count - 1, cs, n1, i+1, p0, p)
        else if not(skip_char(b)) then
          putchars_stripped (pfbuf | params, skip_count, cs, n1, i+1, p0, p)
        else
          // FIXME instead of passing skip_count, could we skip >1 char?
          putchars_stripped (pfbuf | params, 2, cs, n1, i+1, p0, p)
        end
    else
      let
        val () = cbuf_clearall (pfbuf | p0, p)
      in end

%{^
  typedef struct {
    params_t params ;
    ats_ptr_type p_cbuf ;
  } envstdoutq_t ;

  ATSinline()
  ats_void_type
  envstdoutq_initialize (envstdoutq_t *env, params_t *from) {

    extern
    ats_void_type
    mycat_params_copy (ats_ptr_type to, ats_ptr_type from) ;
    mycat_params_copy (&env->params, from) ;
    env->p_cbuf = ATS_MALLOC (CBUFSZ) ;
    return ;

  }

  ATSinline()
  ats_void_type
  envstdoutq_uninitialize (envstdoutq_t *env) {
    ATS_FREE (env->p_cbuf) ; return ;
  }

  ATSinline()
  ats_ptr_type
  envstdoutq_get_cbuf
    (envstdoutq_t *env) { return env->p_cbuf ; }
%}

viewtypedef
envstdoutq =
$extype_struct "envstdoutq_t"
  of { params = params }

extern
fun envstdoutq_initialize (env: &envstdoutq? >> envstdoutq, from: &params ) : void =
  "mac#envstdoutq_initialize"

extern
fun envstdoutq_uninitialize (env: &envstdoutq >> envstdoutq?) : void =
  "mac#envstdoutq_uninitialize"

local
  stadef viewout = $UNSAFE.viewout
in

  extern
  fun envstdoutq_get_cbuf (env: &envstdoutq) : [l0:addr] (viewout (cbuf_v (l0, CBUFSZ, l0)) | ptr l0)
      = "mac#envstdoutq_get_cbuf"

  implement
  putchars<envstdoutq> (env, cs, n1) =
    let
      val (pfout | p0) = envstdoutq_get_cbuf (env)
      prval (pf, fpf) = $UNSAFE.viewout_decode (pfout)
      val () =
        putchars_stripped (pf | env.params, 0, cs, n1, 0, p0, p0)
      prval () = fpf (pf)
    in end

end

fun readout_stripped
  {fd:int} (pf: !fildes_v fd | params: &params, fd: int fd) : void =
    let
      var env1: envinp(fd)
      val () = env1.fildes := fd
      prval () = env1.fildes_v := pf
      var env2: envstdoutq
      val () = envstdoutq_initialize (env2, params)
      #define BUFSZ 4096
      var !p_buf with pf_buf = @[byte][BUFSZ]()
      prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
      val () = catloop<envinp(fd),envstdoutq> (env1, env2, !p_buf, BUFSZ)
      prval () = pf := env1.fildes_v
      val () = envstdoutq_uninitialize (env2)
    in
    end

fun cat_stdin
  (params: &params): void = 
  {
    val (pf_stdin | ()) =
      $UNISTD.stdin_fildes_view_get ()
    val isq = quote_output (params)
    val () =
      if isq then
        readout_stripped (pf_stdin | params, STDIN_FILENO)
      else
        readout_raw (pf_stdin | STDIN_FILENO)
    val () = $UNISTD.stdin_fildes_view_set (pf_stdin|)
  }

// open, dump and close file
fun cat_file (params: &params, path: string) : void =
  let
    val (pf_fd | fd) = $FCNTL.open_flag_exn(path, RDONLY)
    val isq = quote_output (params)
    val () =
      if isq then
        readout_stripped (pf_fd | params, fd)
      else
        readout_raw (pf_fd | fd)
  in
    $FCNTL.close_exn (pf_fd | fd)
  end

fn strip_ansi (params: &params) : void =
  params.strip_ansi := true

#define PROGRAM_VERSION "ats-cat version 0.1.4\nCopyright (c) 2017 Vanessa McHale\n"
fn version() = prerr(PROGRAM_VERSION)

// TODO strip out all the other junk; we mostly don't care.
fn help () = prerr "Usage: ac [OPTION] ... [FILE] ...

Concatenate FILE(s), or standard input, to standard output.
    -s, --strip-ansi         strip ANSI codes
    -V, --version            show version information
    -h, --help               display this help and exit

When no file is provided ac will read standard input.
Examples:
  ac file1 file2  Output f's contents, then g's contents.
  ac              Copy standard input to standard output.
  
Bug reports and updates at github.com/vmchale/fastcat\n"

fun should_help
  {n: int | n >= 1}
  {m: nat | m < n}
  ( argc: int n
  , argv: &(@[string][n])
  , current: int m ) : bool =
  let
    val path = string1_of_string (argv.[current])
  in
    if current < argc - 1 then
      path = "--help" || path = "-h" || should_help(argc, argv, current + 1)
    else
      path = "--help" || path = "-h"
  end

// when the current parameter is not a file path
fun parse_non_file_parameters
  {n:int | n >= 1}
  {m:nat | m < n } 
  ( params: &params
  , argc: int n, argv: &(@[string][n])
  , current: int m ) : bool =
  let
    val () = assertloc(current < argc)
    val param = string1_of_string(argv.[current])
  in
    case param of
      | "--help" => ( help(); exit(0); )
      | "-h" => ( help(); exit(0); )
      | "-V" => ( version(); exit(0); )
      | "--version" => ( version(); exit(0); )
      | "-s" => ( strip_ansi(params); false; )
      | "--strip-ansi" => ( strip_ansi(params); false; )
      | _ => true
end

// suppose that current parameter is a file path to cat()
fun parse_file_path
  {n:int | n >= 1}
  {m:nat | m < n }
  (params: &params, argc: int n, argv: &(@[string][n]), current: int m) : void =
  let
    val () = assertloc(current < argc)
    val path = string1_of_string (argv.[current])
    val () = assert_errmsg_bool1 ( gte_size1_int1(string1_length(path), 1), "path must contain at least one character" )
    val () =
      if not(should_help(argc, argv, 0)) then 
        cat_file(params, path) 
      else ( help() ; exit(0) )
    val next = current+1
  in
    if next = argc then
      ()
  else
    parse_file_path(params, argc, argv, next)
end

and parse_parameters
  {n:int | n >= 1}
  {m:nat | m < n } 
  ( params: &params, with_filepath: bool
  , argc: int n, argv: &(@[string][n])
  , current: int m) : void =
  let
    val isfilepath = parse_non_file_parameters (params, argc, argv, current)
    val next = current + 1
  in
    case isfilepath of
    | false => (
        if next = argc
          then
            if with_filepath
              then ( print("Warning, not a filepath: ") ; print(current) ; print("\n") )
              else cat_stdin(params)
          else parse_parameters(params, with_filepath, argc,argv,next)
      )
    | true => parse_file_path(params, argc,argv,current)
end

implement
main(argc, argv) =
  let
    var params : params
    val () = params.strip_ansi := false
  in
    if argc = 1 then
      cat_stdin(params)
    else
      parse_parameters(params, false, argc, argv, 1)
  end
