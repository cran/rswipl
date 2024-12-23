/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011, University of Amsterdam
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

#ifndef PL_STRING_H_INCLUDED
#define PL_STRING_H_INCLUDED

COMMON(char *)		store_string(const char *s);
COMMON(void)		remove_string(char *s);
COMMON(int)		digitValue(int b, int c);
COMMON(bool)		strprefix(const char *string, const char *prefix);
COMMON(bool)		strpostfix(const char *string, const char *postfix);
COMMON(bool)		stripostfix(const char *string, const char *postfix);
#ifndef HAVE_STRCASECMP
COMMON(int)		strcasecmp(const char *s1, const char *s2);
#endif
#ifndef HAVE_STRLWR
COMMON(char *)		strlwr(char *s);
#endif
#ifndef HAVE_MBSCOLL
COMMON(int)		mbscoll(const char *s1, const char *s2);
#endif
#ifndef HAVE_MBSCASECOLL
COMMON(int)		mbscasecoll(const char *s1, const char *s2);
#endif

/*  Return the character representing some digit.

 ** Fri Jun 10 10:45:40 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static inline char
digitName(int n, int smll)
{ if (n <= 9)
    return (char)(n + '0');
  return (char)(n + (smll ? 'a' : 'A') - 10);
}


#endif /*PL_STRING_H_INCLUDED*/
