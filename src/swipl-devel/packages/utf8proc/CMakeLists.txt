cmake_minimum_required(VERSION 3.10)
project(swipl-utf8proc)

include("../cmake/PrologPackage.cmake")

find_package(PkgConfig REQUIRED)
pkg_check_modules(UTF8PROC IMPORTED_TARGET libutf8proc)
if(TARGET PkgConfig::UTF8PROC)

include(CheckStructHasMember)
include(CheckSymbolExists)
include(CheckCSourceCompiles)

# Propagate include directories and linker flags from the pkg-config
# target so the feature probes below see the right headers/libs.
set(CMAKE_REQUIRED_INCLUDES ${UTF8PROC_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${UTF8PROC_LIBRARIES})

# Fields of utf8proc_property_t.  Older distro builds (Ubuntu LTS)
# ship a utf8proc that predates `charwidth`, `ambiguous_width`, and
# `indic_conjunct_break`.  check_struct_has_member uses offsetof(),
# which ISO C forbids on bitfields, so these are probed with a plain
# compile test that reads each field into a local.
function(utf8proc_has_field FIELD VAR)
  check_c_source_compiles("
#include <utf8proc.h>
int main(void) {
  utf8proc_property_t p = {0};
  unsigned x = p.${FIELD};
  (void)x;
  return 0;
}
" ${VAR})
endfunction()

utf8proc_has_field(charwidth            HAVE_UTF8PROC_CHARWIDTH)
utf8proc_has_field(ambiguous_width      HAVE_UTF8PROC_AMBIGUOUS_WIDTH)
utf8proc_has_field(bidi_mirrored        HAVE_UTF8PROC_BIDI_MIRRORED)
utf8proc_has_field(indic_conjunct_break HAVE_UTF8PROC_INDIC_CONJUNCT_BREAK)

# Enum value — can't use check_symbol_exists (it's not a macro).
check_c_source_compiles("
#include <utf8proc.h>
int main(void) { return UTF8PROC_BOUNDCLASS_EXTENDED_PICTOGRAPHIC; }
" HAVE_UTF8PROC_BOUNDCLASS_EXTENDED_PICTOGRAPHIC)

# Functions.
check_symbol_exists(utf8proc_unicode_version utf8proc.h
		    HAVE_UTF8PROC_UNICODE_VERSION)
check_symbol_exists(utf8proc_codepoint_valid utf8proc.h
		    HAVE_UTF8PROC_CODEPOINT_VALID)
check_symbol_exists(utf8proc_grapheme_break_stateful utf8proc.h
		    HAVE_UTF8PROC_GRAPHEME_BREAK_STATEFUL)
check_symbol_exists(utf8proc_tolower utf8proc.h HAVE_UTF8PROC_TOLOWER)
check_symbol_exists(utf8proc_toupper utf8proc.h HAVE_UTF8PROC_TOUPPER)
check_symbol_exists(utf8proc_totitle utf8proc.h HAVE_UTF8PROC_TOTITLE)

configure_file(config.h.cmake config.h)
include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR})

swipl_plugin(
    utf8proc
    MODULE unicode4pl
    C_SOURCES unicode4pl.c
    C_LIBS PkgConfig::UTF8PROC
    PL_LIBS unicode.pl)

if(MINGW_ROOT)
get_target_mingw_dlls(DirectDLLS plugin_unicode4pl)
add_mingw_indirect_deps(DLLS ${DirectDLLS})
install_dll(${DLLS})
endif()

pkg_doc(utf8proc
	SECTION
	    unicode.pl)

test_libs(utf8proc)

endif()
