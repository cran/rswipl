/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1997-2025, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef PL_GLOBAL_H_INCLUDED
#define PL_GLOBAL_H_INCLUDED
#include "pl-allocpool.h"
#include "pl-mutex.h"
#include "pl-thread.h"
#include "pl-prof.h"
#include "pl-gmp.h"

#ifndef GLOBAL			/* global variables */
#define GLOBAL extern
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module packs SWI-Prolog global data-structures into two structures.
The structure PL_global_data contains all global data that is related to
the state of the system as a  whole, and PL_local_data contains all data
that is related to a Prolog thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	     CODE DATA		*
		 *******************************/

typedef struct
{
#if VMCODE_IS_ADDRESS
  unsigned char   *_dewam_table;	/* decoding table */
  intptr_t	  _dewam_table_offset;	/* offset of 1st */
  void  **_interpreter_jmp_table;	/* interpreters table */
					/* must be last! (why?) */
  code    _wam_table[I_HIGHEST];	/* code --> address */
#else
  int	  struct_may_not_be_empty;	/* empty structure is illegal */
#endif
  struct				/* see initSupervisors() */
  { code exit[3];			/* I_EXIT */
    code next_clause[3];		/* S_NEXTCLAUSE */
    code virgin[3];			/* S_VIRGIN */
    code undef[3];			/* S_UNDEF */
    code dynamic[3];			/* S_DYNAMIC */
    code incr_dynamic[3];		/* S_INCR_DYNAMIC */
    code thread_local[3];		/* S_THREAD_LOCAL */
    code multifile[3];			/* S_MULTIFILE */
    code staticp[3];			/* S_STATIC */
    code wrapper[3];			/* S_WRAP */
    code trie_gen[3];			/* S_TRIE_GEN */
  } supervisors;
} PL_code_data_t;

typedef struct
{ atom_t	file;			/* current source file */
  IOPOS		position;		/* Line, line pos, char and byte */
} source_location;

		 /*******************************
		 *	    GLOBAL DATA		*
		 *******************************/

struct PL_global_data
{ pl_defaults_t	defaults;		/* system default settings */
  pl_options_t	options;		/* command-line options */
  State		stateList;		/* list of loaded states */
  int		initialised;		/* Heap is initialised */
  int		io_initialised;		/* I/O system has been initialised */
  cleanup_status cleaning;		/* Inside PL_cleanup() */
  int		halt_cancelled;		/* Times halt was cancelled */
  int		halt_status;		/* For killing threads */
  unsigned int	bootsession;		/* -b boot compilation */
  int		debug_level;		/* Maintenance debugging: 0..9 */
  struct bit_vector *debug_topics;	/* debug topics enabled */

  struct
  { void *	DB;			/* program resource database */
    atom_t	handle;			/* Symbol for DB */
  } resources;

  struct
  { sig_handler handlers[NUM_SIGNALS];	/* How Prolog receives signals */
#ifdef SIG_ALERT
    int		sig_alert;		/* our alert signal */
#endif
  } signals;

  volatile gen_t _generation;		/* generation of the database */

  struct
  { int		os_argc;		/* main(int argc, char **argv) */
    char **	os_argv;
    int		appl_argc;		/* Application options */
    int		appl_malloc;		/* Array is allocated */
    char **	appl_argv;
    int		notty;			/* -tty: do not use ioctl() */
    int		optimise;		/* -O: optimised compilation */
    int		packs;			/* --no-packs: no packs */
  } cmdline;

  struct
  { char *	CWDdir;
    size_t	CWDlen;
    char *	executable;		/* Running executable */
#ifdef __WINDOWS__
    char *	module;			/* argv[0] module passed */
#endif
  } paths;

  struct
  { size_t	atoms;			/* No. of atoms defined */
    size_t	atom_string_space;	/* # bytes used to store atoms */
    size_t	atom_string_space_freed;/* # bytes in freed atoms */
    size_t	stack_space;		/* # bytes on stacks */
    int		functors;		/* No. of functors defined */
    int		predicates;		/* No. of predicates defined */
    int		modules;		/* No. of modules in the system */
    size_t	clauses;		/* No. clauses */
    size_t	codes;			/* No. of VM codes generated */
    double	user_cputime;		/* User CPU time (whole process) */
    double	system_cputime;		/* Kernel CPU time (whole process) */
    struct
    { int	created;		/* # created hash tables */
      int	destroyed;		/* # destroyed hash tables */
    } indexes;
#ifdef O_ENGINES
    uint64_t	engines_created;	/* # engines created */
    uint64_t	engines_finished;	/* # engines threads */
    uint64_t	threads_created;	/* # threads created */
    uint64_t	threads_finished;	/* # finished threads */
    double	thread_cputime;		/* Total CPU time of threads */
#endif
    int		errors;			/* Printed error messages */
    int		warnings;		/* Printed warning messages */
  } statistics;

#ifdef O_PROFILE
  struct
  { struct PL_local_data *thread;	/* Thread being profiled */
  } profile;
#endif

  struct
  { Module	user;			/* user module */
    Module	system;			/* system predicate module */
  } modules;

  struct
  { TableWP	modules;		/* atom --> module */
  } tables;

#if O_PLMT
  struct				/* Shared table data */
  { struct trie *variant_table;		/* Variant --> table */
    alloc_pool *node_pool;		/* Node allocation pool for tries */
    counting_mutex  mutex;		/* Sync completion */
#ifdef __WINDOWS__
    CONDITION_VARIABLE cvar;
#else
    pthread_cond_t cvar;
#endif
    struct trie_array *waiting;		/* thread --> trie we are waiting for */
  } tabling;
#endif

  struct
  { TableWP	record_lists;		/* Key -> record list */
  } recorded_db;

  struct
  { ArithF     *functions;		/* index --> function */
    size_t	functions_allocated;	/* Size of above array */
  } arith;

  struct
  { size_t	highest;		/* Highest atom index */
    atom_array	array;
    AtomTable	table;			/* hash-table */
    int		lookups;		/* # atom lookups */
    int		cmps;			/* # string compares for lookup */
    int		initialised;		/* atoms have been initialised */
#ifdef O_ATOMGC
    int		gc;			/* # atom garbage collections */
    int		gc_active;		/* Atom-GC is in progress */
    int		rehashing;		/* Atom-rehash in progress */
    size_t	builtin;		/* Locked atoms (atom-gc) */
    size_t	no_hole_before;		/* You won't find a hole before here */
    size_t	margin;			/* # atoms to grow before collect */
    size_t	non_garbage;		/* # atoms for after last AGC */
    int64_t	collected;		/* # collected atoms */
    size_t	unregistered;		/* # candidate GC atoms */
    double	gc_time;		/* Time spent on atom-gc */
    PL_agc_hook_t gc_hook;		/* Current hook */
#endif
    atom_t     *for_code[256];		/* code --> one-char-atom */
    PL_blob_t  *types;			/* registered atom types */
    int		text_rank;		/* next rank for text types */
    int		nontext_rank;		/* next rank for non-text types */
  } atoms;

  struct
  { TablePP	breakpoints;		/* Code -> Breakpoint table */
  } comp;

  struct
  { ExtensionCell _ext_head;		/* head of registered extensions */
    ExtensionCell _ext_tail;		/* tail of this chain */

    InitialiseHandle initialise_head;	/* PL_initialise_hook() */
    InitialiseHandle initialise_tail;
    PL_dispatch_hook_t dispatch_events; /* PL_dispatch_hook() */

    unsigned int  signature;		/* Foreign predicate signature */
    int		  _loaded;		/* system extensions are loaded */
  } foreign;

#ifdef O_BIGNUM
  struct
  { int initialised;			/* is GMP initialised? */
    int keep_alloc_functions;		/* do not change allocation */
  } gmp;
#endif

  struct				/* pl-format.c */
  { TableWP	predicates;
  } format;

  struct
  { TableWP	table;			/* flag key --> flag */
  } flags;

  struct
  { TableWP	table;			/* global (read-only) features */
  } prolog_flag;

  struct
  { size_t	highest;		/* Next index to handout */
    functor_array array;		/* index --> functor */
    FunctorTable table;			/* hash-table */
    int		 rehashing;		/* Table is being rehashed */
  } functors;

  struct
  { Code	catch_exit_address;	/* See findCatchExit() */
  } exceptions;

  struct
  { struct
    { struct event_list *onabort;	/* Thread aborted */
      struct event_list *onerase;	/* erased clause or dbref */
      struct event_list *onbreak;	/* breakpoint change */
      struct event_list *onframefinish; /* Debugged frame finished */
#ifdef O_ENGINES
      struct event_list *onthreadstart;	/* thread start hook */
      struct event_list *onthreadexit;	/* thread exit hook */
#endif
      struct event_list *onuntable;	/* Untable after reload */
    } hook;
  } event;

  struct
  { Table		tmp_files;	/* Known temporary files */
    struct
    { CanonicalDir     *entries;
      unsigned		size;
    } dir_table;			/* Canonical OS dirs */
    char *		myhome;		/* expansion of ~ */
    char *		fred;		/* last expanded ~user */
    char *		fredshome;	/* home of fred */
    OnHalt		on_halt_list;	/* list of onhalt hooks */
    OnHalt		exit_hooks;	/* how to exit from PL_halt() */
    int			halting;	/* process is shutting down */
    int			gui_app;	/* Win32: Application is a gui app */
    IOFUNCTIONS		iofunctions;	/* initial IO functions */
    IOFUNCTIONS		org_terminal;	/* IO+Prolog terminal functions */
  } os;

  struct
  { Procedure	dgarbage_collect1;
    Procedure	catch3;
    Procedure	reset3;
    Procedure	dmeta_call1;		/* $meta_call/1 */
    Procedure	true0;
    Procedure	fail0;
    Procedure	equals2;		/* =/2 */
    Procedure	is2;			/* is/2 */
    Procedure	strict_equal2;		/* ==/2 */
    Procedure	not_strict_equal2;	/* \==/2 */
    Procedure   arg3;			/* arg/3 */
    Procedure	exception_hook5;
    Procedure	print_message2;
    Procedure	foreign_registered2;	/* $foreign_registered/2 */
    Procedure	prolog_trace_interception4;
    Procedure	prolog_break_hook7;	/* prolog:break_hook/7 */
    Procedure	portray;		/* portray/1 */
    Procedure   dcall1;			/* $call/1 */
    Procedure   call1;			/* call/1 */
    Procedure   call3;			/* call/3 */
    Procedure	undefinterc4;		/* $undefined_procedure/4 */
    Procedure   dthread_init0;		/* $thread_init/0 */
    Procedure   dc_call_prolog0;	/* $c_call_prolog/0 */
    Procedure   dinit_goal3;		/* $init_goal/3 */
#ifdef O_ATTVAR
    Procedure	dwakeup1;		/* system:$wakeup/1 */
    Procedure	portray_attvar1;	/* $attvar:portray_attvar/1 */
#endif
    Procedure   comment_hook3;		/* prolog:comment_hook/3 */
    Procedure	tune_gc3;		/* prolog:tune_gc */
    Procedure	trie_gen_compiled2;
    Procedure	trie_gen_compiled3;
    Procedure	exception3;		/* user:exception/3 */
    Procedure	iri_hook4;		/* $iri:iri_hook/4 */
    Procedure	absolute_file_name3;	/* system:absolute_file_name/3 */
    Procedure	c_open_resource3;	/* $rc:c_open_resource/3 */
    Procedure	clear_source_admin1;	/* system:$clear_source_admin/1 */
    Procedure	answer_completion2;	/* $tabling:answer_completion/2 */
    Procedure	update7;		/* $tabling:update/7 */
    Procedure	answer_count_restraint0;/* $tabling:answer_count_restraint/0 */
    Procedure	radial_restraint0;	/* $tabling:radial_restraint/0 */
    Procedure	tripwire3;		/* $tabling:tripwire/3 */
#if O_ENGINES
    Procedure	signal_is_blocked1;	/* $syspreds:signal_is_blocked1/1 */
#endif
#if O_PLMT
    Procedure	dgc0;			/* system:$gc/0 */
#endif
    Procedure	drun_undo1;		/* $syspreds:$run_undo/1 */
    Procedure	drun_initialization2;	/* system:$run_initialization/2 */
    Procedure	dtranslated_source2;	/* system:$translated_source/2 */
    Procedure	findall_loop4;		/* $bags:findall_loop/4 */
    Procedure	heartbeat0;		/* prolog:heartbeat/0 */

    int		static_dirty;		/* #static dirty procedures */
#ifdef O_CLAUSEGC
    TablePP	dirty;			/* Table of dirty procedures */
#endif
  } procedures;

  struct				/* see raiseInferenceLimitException() */
  { int		initialized;
    predicate_t	not_exceed_[6];
  } inference_limit;

  struct
  { ClauseRef	lingering;		/* Unlinked clause refs */
    size_t	lingering_count;	/* # Unlinked clause refs */
    int		cgc_active;		/* CGC is running */
    int64_t	cgc_count;		/* # clause GC calls */
    int64_t	cgc_reclaimed;		/* # clauses reclaimed */
    double	cgc_time;		/* Total time spent in CGC */
    size_t	dirty;			/* # dirty clauses */
    size_t	erased;			/* # erased pending clauses */
    size_t	erased_size;		/* memory used by them */
    size_t	erased_size_last;	/* memory used by them after last CGC */
    size_t	db_erased_refs;		/* Clause references on erased clauses */
    int		cgc_space_factor;	/* Max total/margin garbage */
    double	cgc_stack_factor;	/* Price to scan stack space */
    double	cgc_clause_factor;	/* Pce to scan clauses */
    Clause	top_clause;		/* See PL_open_query() */
    struct clause_ref top_cref;		/* Its reference */
  } clauses;

  struct
  { float	min_speedup;
    float	max_var_fraction;
    float	min_speedup_ratio;
    int		max_lookahead;
    int		min_clauses;
  } clause_index;

  struct
  { size_t	highest;		/* highest source file index */
    size_t	no_hole_before;		/* All filled before here */
    srcfile_array array;		/* index --> file */
    TableWP	table;			/* name  --> file */
  } files;

#ifdef HAVE_TGETENT
  struct
  { int    initialised;			/* initialisation status */
    char  *_string_area;		/* static area for tgetstr */
    char  *_string_area_end;		/* end of _string_area */
    char  *_buf_area;			/* static area for tgetent */
    TableWP _capabilities;		/* User-level capability table */
  } terminal;
#endif

#ifdef O_ENGINES
  struct
  { PL_thread_info_t   *free;		/* Free threads */
    int			highest_allocated; /* Highest with info struct */
    int			thread_max;	/* Size of threads array */
    int			highest_id;	/* Highest Id of life thread  */
    int			peak_id;	/* Highest Id of any thread  */
    PL_thread_info_t  **threads;	/* Pointers to thread-info */
#ifdef __WINDOWS__
    HINSTANCE		instance;	/* Win32 process instance */
#endif
#ifdef O_PLMT
    int			enabled;	/* threads are enabled */
    int			mutex_next_id;	/* next id for anonymous mutexes */
    TableWP		mutexTable;	/* Name --> mutex table */
    counting_mutex     *mutexes;	/* Registered mutexes */
    struct
    { pthread_mutex_t	mutex;
      pthread_cond_t	cond;
      unsigned int	requests;
      unsigned int	initialized;	/* mutex and condvar are initialized */
    } gc;
    struct
    { pthread_mutex_t	mutex;
      pthread_cond_t	cond;
    } index;
    linger_list	       *lingering;
#endif
  } thread;
#endif /*O_ENGINES*/

  struct
  { functor_t dict_functors[CACHED_DICT_FUNCTORS];
  } dict;

#ifdef O_LOCALE
  struct
  { TableWP		localeTable;	/* Name --> locale table */
    PL_locale	       *default_locale;	/* System wide default */
  } locale;
#endif

  struct
  { int tz_initialized;			/* time zone is initialized */
  } date;

  struct stack		combined_stack; /* ID for combined stack */
};


		 /*******************************
		 *	     LOCAL DATA		*
		 *******************************/

#define LD_MAGIC	0x3cfd82b4	/* Valid local-data structure */

struct PL_local_data
{ uintptr_t	magic;			/* LD_MAGIC */
  LocalFrame    environment;		/* Current local frame */
  Choice	choicepoints;		/* Choice-point chain */
  FliFrame      foreign_environment;	/* Current foreign context */
  QueryFrame    query;			/* Currently open query */
  Word		mark_bar;		/* Mark globals > this one */
#ifdef O_GVAR
  Word		frozen_bar;		/* Frozen part of the global stack */
#endif
  Code		fast_condition;		/* Fast condition support */
  pl_stacks_t   stacks;			/* Prolog runtime stacks */
  int		alerted;		/* Special mode. See updateAlerted() */
  int		slow_unify;		/* do not use inline unification */
  int		critical;		/* heap is being modified */
  int		break_level;		/* current break level */
  Stack		outofstack;		/* thread is out of stack */
  int		trim_stack_requested;	/* perform a trim-stack */
  int		in_print_message;	/* Inside printMessage() */
  void *	glob_info;		/* pl-glob.c */
  IOENC		encoding;		/* default I/O encoding */
  struct PL_local_data *next_free;	/* see maybe_free_local_data() */

  struct
  { wsigmask_t	pending;		/* PL_raise() pending signals */
#if STDC_CV_ALERT /* use C11 condition variable/mutex for thread signalling */
    cnd_t	alert_cv;		/* notify when a signal is in pending */
    mtx_t	alert_mtx;		/* lock on this while waiting for _cv */
#endif
    int		current;		/* currently processing signal */
    int		is_sync;		/* current signal is synchronous */
#ifndef __unix__
    int		forced;			/* Forced signal */
#endif
#ifdef O_C_STACK_GUARDED
    jmp_buf	context;		/* Guarded for C-stack overflows */
    int		sig_critical;		/* context is valid */
    void       *alt_stack;		/* The stack */
#endif
  } signal;

  struct
  { int		active;			/* doing pipe I/O */
    jmp_buf	context;		/* context of longjmp() */
  } pipe;

  struct
  { char       *getstr_buffer;		/* getString() buffer */
    size_t	getstr_buffer_size;	/* size of getstr_buffer */
    struct wic_state *write_state;	/* qlf-write state */
    struct wic_state *read_state;	/* qlf-read state */
  } qlf;

  struct
  { atom_t	current;		/* current global prompt */
    atom_t	first;			/* how to prompt first line */
    int		first_used;		/* did we do the first line? */
    int		next;			/* prompt on next read operation */
  } prompt;

  source_location read_source;		/* file, line, char of last term */

  struct
  { term_t	term;			/* exception term */
    term_t	bin;			/* temporary handle for exception */
    term_t	printed;		/* already printed exception */
    term_t	tmp;			/* tmp for errors */
    term_t	pending;		/* used by the debugger */
    term_t	fr_rewritten;		/* processed by exception_hook() */
    int		in_hook;		/* inside exception_hook() */
    int		processing;		/* processing an exception */
    exception_frame *throw_environment;	/* PL_throw() environments */
  } exception;

#ifdef O_ATTVAR
  struct
  { term_t	head;			/* Head of wakeup list */
    term_t	tail;			/* Tail of this list */
    term_t	gc_attvars;		/* place for attvars during GC */
    Word	attvars;		/* linked list of all attvars */
    int		call_residue_vars_count; /* # call_residue_vars/2 active */
  } attvar;
#endif

  struct
  { term_t	dummy;			/* see trimStacks() */
  } trim;

  struct
  { term_t	h[TMP_PTR_SIZE];	/* temporary handles. See unify_ptrs */
    int		top;			/* Top-of-stack index */
  } tmp;

#ifdef O_GVAR
  struct
  { Table	nb_vars;		/* atom --> value */
    int		grefs;			/* references to global stack */
  } gvar;
#endif

  struct
  { uint64_t	fired;			/* autoyielding check */
    uint64_t	frequency;		/* How often do we fire */
  } yield;

#ifdef __EMSCRIPTEN__
  struct
  { term_t yield_request;		/* See '$await'/2 in wasm/pl-wasm.c */
    term_t yield_result;
    bool   yield_unified;
  } wasm;
#endif

  struct
  { uint64_t	inferences;		/* inferences in this thread */
    uint64_t	last_cputime;		/* milliseconds last CPU time */
    uint64_t	last_systime;		/* milliseconds last SYSTEM time */
    uint64_t	last_real_time;		/* Last Real Time (seconds since Epoch) */
    double	start_time;		/* When Thread was started */
    double	last_walltime;		/* Last Wall time (m-secs since start) */
    double	user_cputime;		/* User saved CPU time */
    double	system_cputime;		/* Kernel saved CPU time */
    int		errors;			/* Printed error messages */
    int		warnings;		/* Printed warning messages */
  } statistics;

#ifdef O_BIGNUM
  struct
  { ar_context *context;		/* current allocation context */
    size_t	max_integer_size;	/* Max size of MPZ data */
  } gmp;
#endif

#ifdef O_PROFILE
  struct
  { int		accounting;		/* we are accounting */
    int		sum_ok;			/* siblings are counted */
    prof_status	active;			/* profiler is on */
    prof_control ports_control;		/* which port counts are generated? */
    unsigned int sample_period;		/* profile sample period (usecs.) */
    struct call_node *current;		/* `current' node */
    struct call_node *roots;		/* list of root-nodes */
    uint64_t	samples;		/* profile samples */
    uint64_t	ticks;			/* profile ticks total */
    uint64_t	accounting_ticks;	/* Ticks in profCall() and friends */
    size_t	nodes;			/* #Recorded nodes */
    double	time_at_last_tick;	/* Time at last statistics tick */
    double	time_at_start;		/* Time at last start */
    double	time;			/* recorded CPU time */
  } profile;
#endif /* O_PROFILE */

  struct
  { Module	typein;			/* module for type in goals */
    Module	source;			/* module we are reading clauses in */
  } modules;

  struct
  { intptr_t	generator;		/* See PL_atom_generator() */
    atom_t	unregistering;		/* See PL_unregister_atom() */
    int		gc_active;		/* Thread is running atom-gc */
  } atoms;

  struct
  { VarDef *	vardefs;		/* compiler variable analysis */
    int		nvardefs;
    int		filledVars;
  } comp;

  struct
  { Buffer	buffered;		/* Buffered events */
    int		delay_nesting;		/* How deeply is delay nested? */

#ifdef O_ENGINES
    struct
    { struct event_list *onthreadexit;	/* thread exit hook */
    } hook;
#endif
  } event;

  struct
  { struct
    { Number	base;
      Number	top;
      Number	max;
    } stack;
#ifdef O_BIGNUM
    struct
    { gmp_randstate_t state;
      int initialised;
    } random;
    struct
    { size_t max_rational_size;
      atom_t max_rational_size_action;
    } rat;
#endif
    struct
    { unsigned int flags;		/* FTL_* */
    } f;
  } arith;

#if O_CYCLIC
  struct
  { segstack lstack;			/* Stack for cycle-links */
    segstack vstack;			/* Stack for visited marks */
  } cycle;
#endif

  struct
  { struct tbl_component *component;    /* active component */
    struct trie *variant_table;		/* Variant --> table */
    alloc_pool *node_pool;		/* Node allocation pool for tries */
    int	has_scheduling_component;	/* A leader was created */
    int in_answer_completion;		/* Running answer completion */
    int in_assert_propagation;		/* Running propagate_assert/1 */
    unsigned int flags;			/* Global flags (TF_*) */
    term_t delay_list;			/* Global delay list */
    term_t idg_current;			/* Current node in IDG (trie symbol) */
    struct
    { atom_t max_table_subgoal_size_action;
      size_t max_table_subgoal_size;
      atom_t max_table_answer_size_action;
      size_t max_table_answer_size;
      atom_t max_answers_for_subgoal_action;
      size_t max_answers_for_subgoal;
    } restraint;
  } tabling;

  struct
  {
#ifdef __BEOS__
    status_t	dl_error;		/* dlopen() emulation in pl-beos.c */
#endif
    int		rand_initialised;	/* have we initialised random? */
#ifdef O_DDE
    unsigned	dde_instance;		/* Actually DWORD */
#endif
  } os;

  struct
  { TableWP	  table;		/* local Prolog flag table */
    pl_features_t mask;			/* Masked access to booleans */
    int		  write_attributes;	/* how to write attvars? */
    occurs_check_t occurs_check;	/* Unify and occurs check */
    access_level_t access_level;	/* Current access level */
    unsigned int   unknown_option;	/* OPT_UNKNOWN_* */
  } prolog_flag;

  struct
  { FindData	find;			/* /<ports> <goal> in tracer */
  } trace;

  struct findall_state *bags;		/* findall/3 store  */

  struct
  { AbortHandle	_abort_head;		/* PL_abort_hook() */
    AbortHandle _abort_tail;

    buffer	_discardable_buffer;	/* PL_*() character buffers */
    string_stack string_buffers;	/* PL_*() string buffers */

    int		SP_state;		/* For SICStus interface */
  } fli;

  struct				/* Local IO stuff */
  { IOSTREAM *streams[6];		/* handles for standard streams */
    st_check stream_type_check;		/* Check bin/text streams? */
					/* do not copy from parent */
    struct input_context *input_stack;	/* maintain input stream info */
    struct output_context *output_stack; /* maintain output stream info */
    int	portray_nesting;		/* depth of portray nesting */
  } IO;

  struct
  { fid_t	numbervars_frame;	/* Numbervars choice-point */
  } var_names;

#ifdef O_LIMIT_DEPTH
  struct
  { size_t limit;
    size_t reached;
  } depth_info;
#endif

#ifdef O_INFERENCE_LIMIT
  struct
  { int64_t limit;			/* Raise at this count */
  } inference_limit;
#endif

  definition_refs predicate_references;	/* Referenced predicates */

  pl_shift_status_t shift_status;	/* Stack shifter status */
  pl_debugstatus_t _debugstatus;	/* status of the debugger */
  struct btrace *btrace_store;		/* C-backtraces */
#if O_DEBUG
  pl_internaldebugstatus_t internal_debug; /* status of C-level debug flags */
#endif

#ifdef O_ENGINES
  struct
  { struct _PL_thread_info_t *info;	/* info structure */
    uint64_t seq_id;			/* Sequence id (Unique) */
					/* Communication */
    message_queue messages;		/* Message queue */
    struct _thread_sig   *sig_head;	/* Head of signal queue */
    struct _thread_sig   *sig_tail;	/* Tail of signal queue */
    DefinitionChain local_definitions;	/* P_THREAD_LOCAL predicates */
    int magic;				/* PL_THREAD_MAGIC (checking) */
    int exit_requested;			/* Thread is asked to exit */
    struct _PL_thread_info_t *creator;	/* Thread that created me */
    uint64_t creator_seq_id;		/* Seq id of creater */
#ifdef O_PLMT
    simpleMutex scan_lock;		/* Hold for asynchronous scans */
    thread_wait_for *waiting_for;	/* thread_wait/2 info */
    alert_channel alert;		/* How to alert the thread */
    double child_cputime;		/* Time of completed children */
    uint64_t child_inferences;		/* Inferences in children */
#endif
  } thread;
#endif

  struct
  { gen_t	      gen_start;	/* Global start generation */
    gen_t	      gen_base;		/* Local  start generation */
    gen_t	      gen_max;		/* Transaction max gen */
    gen_t	      gen_nest;		/* Start of nested generation */
    gen_t	      generation;	/* Local current generation */
    TablePW	      clauses;		/* Affected clauses */
    TablePW	      predicates;	/* Pred --> last modified */
    struct tbl_trail *table_trail;	/* Affected tables */
    term_t	      id;		/* Default the goal */
    struct tr_stack  *stack;		/* Nested transaction stack */
    unsigned int      flags;		/*  */
  } transaction;

#ifdef O_LOCALE
  struct
  { PL_locale *current;			/* Current locale */
  } locale;
#endif

  struct
  { size_t	erased_skipped;		/* # erased clauses skipped */
    int64_t	cgc_inferences;		/* Inferences at last cgc consider */
  } clauses;

#ifdef O_COVERAGE
  struct
  { struct coverage *data;		/* Data for the coverage collection */
    int		     active;		/* This thread is recording */
  } coverage;
#endif

  struct
  { gen_t	generation;		/* reload generation */
    int		nesting;		/* reload nesting */
  } reload;

  struct
  { DefinitionChain nesting;		/* Nesting chain in the autoloader */
    Definition	loop;			/* We are looping on this def */
  } autoload;

  struct
  { term_t undo_list;			/* Stacked undo goals */
    Buffer scheduled;
    int    running;
  } undo;

  struct
  { size_t _total_marked;		/* # marked global cells */
    size_t _trailcells_deleted;		/* # garbage trailcells */
    size_t _relocation_chains;		/* # relocation chains (debugging) */
    size_t _relocation_cells;		/* # relocation cells */
    size_t _relocated_cells;		/* # relocated cells */
    size_t _needs_relocation;		/* # cells that need relocation */
    size_t _local_marked;		/* # marked local -> global ptrs */
    size_t _marks_swept;		/* # marks swept */
    size_t _marks_unswept;		/* # marks swept */
    size_t _alien_relocations;		/* # alien_into_relocation_chain() */
    size_t _local_frames;		/* frame count for debugging */
    size_t _choice_count;		/* choice-point count for debugging */
    int  *_start_map;			/* bitmap with legal global starts */
    sigset_t saved_sigmask;		/* Saved signal mask */
    int64_t inferences;			/* #inferences at last GC */
    pl_gc_status_t	status;		/* Garbage collection status */
#ifdef O_CALL_RESIDUE
    int			marked_attvars;	/* do not GC attvars */
#endif
    int active;				/* GC is running in this thread */
    gc_stats stats;			/* GC performance history */

					/* These must be at the end to be */
					/* able to define O_DEBUG in only */
					/* some modules */
#if defined(O_DEBUG) || defined(SECURE_GC)
    intptr_t _trailtops_marked;		/* # marked trailtops */
    Word *_mark_base;			/* Array of marked cells addresses */
    Word *_mark_top;			/* Top of this array */
    TablePW _check_table;		/* relocation address table */
    TablePW _local_table;		/* marked local variables */
    int  _relocated_check;		/* Verify relocated addresses? */
    unsigned int incr_seed;		/* Seed for random stack increments */
#endif
  } gc;
};

GLOBAL PL_global_data_t PL_global_data;
GLOBAL PL_code_data_t	PL_code_data;
GLOBAL PL_local_data_t  PL_local_data;
#if defined(O_ENGINES) && !defined(O_PLMT)
GLOBAL PL_local_data_t *PL_current_engine_ptr;
#endif

#define GD (&PL_global_data)
#define CD (&PL_code_data)

#define functor_array		(GD->functors.array)
#define systemDefaults		(GD->defaults)

#define environment_frame	(LD->environment)
#define fli_context		(LD->foreign_environment)
#define source_file_name	(LD->read_source.file)
#define source_line_no		(LD->read_source.position.lineno)
#define source_line_pos		(LD->read_source.position.linepos)
#define source_char_no		(LD->read_source.position.charno)
#define source_byte_no		(LD->read_source.position.byteno)
#define exception_term		(LD->exception.term)
#define exception_bin		(LD->exception.bin)
#define exception_printed	(LD->exception.printed)
#define gc_status		(LD->gc.status)
#define debugstatus		(LD->_debugstatus)
#define Suser_input		(LD->IO.streams[SNO_USER_INPUT])
#define Suser_output		(LD->IO.streams[SNO_USER_OUTPUT])
#define Suser_error		(LD->IO.streams[SNO_USER_ERROR])
#define Scurin			(LD->IO.streams[SNO_CURRENT_INPUT])
#define Scurout			(LD->IO.streams[SNO_CURRENT_OUTPUT])
#define Sprotocol		(LD->IO.streams[SNO_PROTOCOL])
#define Sdin			Suser_input		/* not used for now */
#define Sdout			Suser_error

#ifdef VMCODE_IS_ADDRESS
#define dewam_table		(CD->_dewam_table)
#define dewam_table_offset	(CD->_dewam_table_offset)
#define wam_table		(CD->_wam_table)
#define interpreter_jmp_table	(CD->_interpreter_jmp_table)
#endif /*VMCODE_IS_ADDRESS*/

#endif /*PL_GLOBAL_H_INCLUDED*/
