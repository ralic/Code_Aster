subroutine amdbar(n, pe, iw, len, iwlen,&
                  pfree, nv, next, last, head,&
                  elen, degree, ncmpa, w, iovflo)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: olivier.boiteau at edf.fr
! TOLE CRP_20
    implicit none
!
!---------------------------------------------------------------
!  THE MC47 / AMD SUITE OF MINIMUM DEGREE ORDERING ALGORITHMS.
!
!  THIS CODE IS ONE OF SEVEN VARIATIONS OF A SINGLE ALGORITHM:
!  THE PRIMARY ROUTINE (MC47B/BD, ONLY AVAILABLE IN THE HARWELL
!  SUBROUTINE LIBRARY), AND 6 VARIATIONS THAT DIFFER ONLY IN
!  HOW THEY COMPUTE THE DEGREE (AVAILABLE IN NETLIB).
!
!  FOR INFORMATION ON THE HARWELL SUBROUTINE LIBRARY, CONTACT
!  MARIA WOODBRIDGE,
!  AEA TECHNOLOGY PRODUCTS AND SYSTEMS
!  HARWELL, DIDCOT, OXFORDSHIRE OX11 0RA,
!  TELEPHONE (44) 1235 432345,
!  FAX       (44) 1235 432023,
!  EMAIL     MARIA.WOODBRIDGE, AT , AEAT.CO.UK
!------------------------------------------------------------------
!
!********************************************************************
! NOTICE:
!------------------------------------------------------------------
! CADRE GENERAL D'UTILISATION DE LA BIBLIOTHEQUE HARWELL
!------------------------------------------------------------------
! THE AMD ROUTINES (AMDEXA, AMDBAR, AMDHAF, AMDHAT, AMDTRU,
! AND AMDATR) MAY BE USED SOLELY FOR EDUCATIONAL, RESEARCH, AND
! BENCHMARKING PURPOSES BY NON-PROFIT ORGANIZATIONS AND THE U.S.
! GOVERNMENT.  COMMERCIAL AND OTHER ORGANIZATIONS MAY MAKE USE OF THE
! AMD ROUTINES SOLELY FOR BENCHMARKING PURPOSES ONLY.  THE AMD
! ROUTINES MAY BE MODIFIED BY OR ON BEHALF OF THE USER FOR SUCH
! USE BUT AT NO TIME SHALL THE AMD ROUTINES OR ANY SUCH MODIFIED
! VERSION OF THEM BECOME THE PROPERTY OF THE USER.  THE AMD ROUTINES
! ARE PROVIDED WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR
! IMPLIED.  NEITHER THE AUTHORS NOR THEIR EMPLOYERS SHALL BE LIABLE
! FOR ANY DIRECT OR CONSEQUENTIAL LOSS OR DAMAGE WHATSOEVER ARISING
! OUT OF THE USE OR MISUSE OF THE AMD ROUTINES BY THE USER.  THE AMD
! ROUTINES MUST NOT BE SOLD.  YOU MAY MAKE COPIES OF THE AMD ROUTINES,
! BUT THIS NOTICE AND THE COPYRIGHT NOTICE MUST APPEAR IN ALL COPIES.
! ANY OTHER USE OF THE AMD ROUTINES REQUIRES WRITTEN PERMISSION.
! YOUR USE OF THE AMD ROUTINES IS AN IMPLICIT AGREEMENT TO THESE
! CONDITIONS.
!------------------------------------------------------------------
! UTILISATION POUR DES PRESTATIONS DE LA DIVISION R&D
!------------------------------------------------------------------
! EDF - POLE INDUSTRIE - DIVISION RECHERCHE ET DEVELOPPEMENT
! SERVICE ER - DEPARTEMENT MOS - (ARNAUD RENAUD)
! A ACQUIS EN 1997 UNE LICENCE POUR LE SITE DE CLAMART
! (SITE MEANS PREMISES LOCATED AT ONE POSTAL ADDRESS)
! DE L'ENSEMBLE DE LA LIBRAIRIE HARWELL (VERSION 12).
! C'EST UNE "STANDARD" LICENCE (PAS SEULEMENT "ACADEMIC")
! UTILISABLE A DES FINS COMMERCIALES PAR LES UNITES DE CE SITE.
!------------------------------------------------------------------
! UTILISATION POUR DIFFUSION DE LOGICIEL
!------------------------------------------------------------------
! EDF - POLE INDUSTRIE - DIVISION RECHERCHE ET DEVELOPPEMENT
! SERVICE IMA - DEPARTEMENT MMN - (PASCAL ESPOSITO)
! A ACQUIS EN MARS 1999 UNE LICENCE DE LA ROUTINE MC47 DE
! LA BIBLIOTHEQUE HARWELL (VERSION 12, DECEMBRE 1995) :
! 1- POUR UTILISATION DANS LE PROGICIEL CODE_ASTER
! 2- POUR DIFFUSION A DES TIERCES PARTIES DU PROGICIEL INCLUANT CETTE
!    ROUTINE SOUS FORME DE CODE OBJET
! CETTE LICENCE EST PERMANENTE (SANS LIMITATION DE DATE), POUR TOUTE
! PLATE-FORME INFORMATIQUE, EN TOUT LIEU.
!
! ********************************************************************
!-----------------------------------------------------------------------
! AMDBAR:  APPROXIMATE MINIMUM (UMFPACK/MA38-STYLE, EXTERNAL) DEGREE
!          ORDERING ALGORITHM, BUT WITHOUT AGGRESIVE ABSORPTION
!-----------------------------------------------------------------------
!
!  VARIATION 2:  MC47-STYLE APPROXIMATE EXTERNAL DEGREE, BUT WITH NO
!  AGGRESIVE ABSORPTION.  THIS IS INCLUDED FOR COMPARISON WITH THE
!  OTHER 5 VARIATIONS.  IT TENDS TO COMPUTE ORDERINGS COMPARABLE TO
!  MC47B/BD, OR SLIGHTLY WORSE IN SOME CASES.  IT TENDS TO BE ABOUT AS
!  FAST AS MC47B/BD.
!
!  WE RECOMMEND USING MC47B/BD INSTEAD OF THIS ROUTINE SINCE MC47B/BD
!  GIVES BETTER RESULTS IN ABOUT THE SAME TIME.
!
!-----------------------------------------------------------------------
!
! GIVEN A REPRESENTATION OF THE NONZERO PATTERN OF A SYMMETRIC MATRIX,
!       A, (EXCLUDING THE DIAGONAL) PERFORM AN APPROXIMATE MINIMUM
!       (UMFPACK/MA38-STYLE) DEGREE ORDERING TO COMPUTE A PIVOT ORDER
!       SUCH THAT THE INTRODUCTION OF NONZEROS (FILL-IN) IN THE CHOLESKY
!       FACTORS A = LLT ARE KEPT LOW.  AT EACH STEP, THE PIVOT
!       SELECTED IS THE ONE WITH THE MINIMUM UMFAPACK/MA38-STYLE
!       UPPER-BOUND ON THE EXTERNAL DEGREE.  THIS ROUTINE DOES NOT
!       PERFORM AGGRESIVE ABSORPTION (AS DONE BY MC47B/BD).  AGGRESIVE
!       ABSORPTION IN MC47B/BD IS USED TO TIGHTEN THE BOUND ON THE
!       DEGREE.  THIS CAN RESULT AN SIGNIFICANT IMPROVEMENT IN THE
!       QUALITY OF THE ORDERING FOR SOME MATRICES.
!
!       THE APPROXIMATE DEGREE ALGORITHM IMPLEMENTED HERE IS THE
!       SYMMETRIC ANALOG OF THE DEGREE UPDATE ALGORITHM IN MA38 AND
!       UMFPACK (THE UNSYMMETRIC-PATTERN MULTIFRONTAL PACKAGE, BOTH BY
!       DAVIS AND DUFF, AVAILABLE FOR ACADEMIC USERS IN NETLIB AS
!       LINALG/UMFPACK.SHAR OR VIA ANONYMOUS FTP TO
!       FTP.CIS.UFL.EDU:PUB/UMFPACK).  NON-ACADEMIC USERS MUST USE
!       MA38 IN THE HARWELL SUBROUTINE LIBRARY INSTEAD OF UMPFACK.
!
! **********************************************************************
! ***** CAUTION:  ARGUMENTS ARE NOT CHECKED FOR ERRORS ON INPUT.  ******
! **********************************************************************
! ** IF YOU WANT ERROR CHECKING, A MORE VERSATILE INPUT FORMAT, AND A **
! ** SIMPLER USER INTERFACE, THEN USE MC47A/AD IN THE HARWELL         **
! ** SUBROUTINE LIBRARY, WHICH CHECKS FOR ERRORS, TRANSFORMS THE      **
! ** INPUT, AND CALLS MC47B/BD.                                       **
! **********************************************************************
!
!       REFERENCES:  (UF TECH REPORTS ARE AVAILABLE VIA ANONYMOUS FTP
!       TO FTP.CIS.UFL.EDU:CIS/TECH-REPORTS).
!
!       (1) TIMOTHY A. DAVIS AND IAIN DUFF, AN UNSYMMETRIC-PATTERN
!               MULTIFRONTAL METHOD FOR SPARSE LU FACTORIZATION,
!               SIAM J. MATRIX ANALYSIS AND APPLICATIONS, TO APPEAR.
!               ALSO UNIV. OF FLORIDA TECHNICAL REPORT TR-94-038.
!               DISCUSSES UMFPACK / MA38.
!
!       (2) PATRICK AMESTOY, TIMOTHY A. DAVIS, AND IAIN S. DUFF,
!               AN APPROXIMATE MINIMUM DEGREE ORDERING ALGORITHM,
!               SIAM J. MATRIX ANALYSIS AND APPLICATIONS (TO APPEAR),
!               ALSO UNIV. OF FLORIDA TECHNICAL REPORT TR-94-039.
!               DISCUSSES THIS ROUTINE.
!
!       (3) ALAN GEORGE AND JOSEPH LIU, THE EVOLUTION OF THE
!               MINIMUM DEGREE ORDERING ALGORITHM, SIAM REVIEW, VOL.
!               31, NO. 1, PP. 1-19, MARCH 1989.  WE LIST BELOW THE
!               FEATURES MENTIONED IN THAT PAPER THAT THIS CODE
!               INCLUDES:
!
!       MASS ELIMINATION:
!               YES.  MA27 RELIED ON SUPERVARIABLE DETECTION FOR MASS
!               ELIMINATION.
!       INDISTINGUISHABLE NODES:
!               YES (WE CALL THESE "SUPERVARIABLES").  THIS WAS ALSO IN
!               THE MA27 CODE - ALTHOUGH WE MODIFIED THE METHOD OF
!               DETECTING THEM (THE PREVIOUS HASH WAS THE TRUE DEGREE,
!               WHICH WE NO LONGER KEEP TRACK OF).  A SUPERVARIABLE IS
!               A SET OF ROWS WITH IDENTICAL NONZERO PATTERN.  ALL
!               VARIABLES IN A SUPERVARIABLE ARE ELIMINATED TOGETHER.
!               EACH SUPERVARIABLE HAS AS ITS NUMERICAL NAME THAT OF
!               ONE OF ITS VARIABLES (ITS PRINCIPAL VARIABLE).
!       QUOTIENT GRAPH REPRESENTATION:
!               YES.  WE USE THE TERM "ELEMENT" FOR THE CLIQUES FORMED
!               DURING ELIMINATION.  THIS WAS ALSO IN THE MA27 CODE.
!               THE ALGORITHM CAN OPERATE IN PLACE, BUT IT WILL WORK
!               MORE EFFICIENTLY IF GIVEN SOME "ELBOW ROOM."
!       ELEMENT ABSORPTION:
!               YES.  THIS WAS ALSO IN THE MA27 CODE.
!       EXTERNAL DEGREE:
!               YES.  THE MA27 CODE WAS BASED ON THE TRUE DEGREE.
!       INCOMPLETE DEGREE UPDATE AND MULTIPLE ELIMINATION:
!               NO.  THIS WAS NOT IN MA27, EITHER.  OUR METHOD OF
!               DEGREE UPDATE WITHIN MC47B/BD IS ELEMENT-BASED, NOT
!               VARIABLE-BASED.  IT IS THUS NOT WELL-SUITED FOR USE
!               WITH INCOMPLETE DEGREE UPDATE OR MULTIPLE ELIMINATION.
!
!-----------------------------------------------------------------------
! AUTHORS, AND COPYRIGHT (C) 1995 BY:
!       TIMOTHY A. DAVIS, PATRICK AMESTOY, IAIN S. DUFF, & JOHN K. REID.
!
! ACKNOWLEDGEMENTS:
!       THIS WORK (AND THE UMFPACK PACKAGE) WAS SUPPORTED BY THE
!       NATIONAL SCIENCE FOUNDATION (ASC-9111263 AND DMS-9223088).
!       THE UMFPACK/MA38 APPROXIMATE DEGREE UPDATE ALGORITHM, THE
!       UNSYMMETRIC ANALOG WHICH FORMS THE BASIS OF MC47B/BD, WAS
!       DEVELOPED WHILE TIM DAVIS WAS SUPPORTED BY CERFACS (TOULOUSE,
!       FRANCE) IN A POST-DOCTORAL POSITION.
!
! DATE:  SEPTEMBER, 1995
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
! INPUT ARGUMENTS (UNALTERED):
!-----------------------------------------------------------------------
!
! N:    THE MATRIX ORDER.
!
!       RESTRICTION:  1 .LE. N .LT. (IOVFLO/2)-2
!
! IWLEN:        THE LENGTH OF IW (1..IWLEN).  ON INPUT, THE MATRIX IS
!       STORED IN IW (1..PFREE-1).  HOWEVER, IW (1..IWLEN) SHOULD BE
!       SLIGHTLY LARGER THAN WHAT IS REQUIRED TO HOLD THE MATRIX, AT
!       LEAST IWLEN .GE. PFREE + N IS RECOMMENDED.  OTHERWISE,
!       EXCESSIVE COMPRESSIONS WILL TAKE PLACE.
!       *** WE DO NOT RECOMMEND RUNNING THIS ALGORITHM WITH ***
!       ***      IWLEN .LT. PFREE + N.                      ***
!       *** BETTER PERFORMANCE WILL BE OBTAINED IF          ***
!       ***      IWLEN .GE. PFREE + N                       ***
!       *** OR BETTER YET                                   ***
!       ***      IWLEN .GT. 1.2 * PFREE                     ***
!       *** (WHERE PFREE IS ITS VALUE ON INPUT).            ***
!       THE ALGORITHM WILL NOT RUN AT ALL IF IWLEN .LT. PFREE-1.
!
!       RESTRICTION: IWLEN .GE. PFREE-1
!
! IOVFLO:       THE LARGEST POSITIVE INTEGER THAT YOUR COMPUTER CAN
!       REPRESENT (-IOVFLO SHOULD ALSO BE REPRESENTABLE).  ON A 32-BIT
!       COMPUTER WITH 2'S-COMPLEMENT ARITHMETIC,
!       IOVFLO = (2 PUISSANCE 31)-1 = 2,147,483,648.
!
!-----------------------------------------------------------------------
! INPUT/OUPUT ARGUMENTS:
!-----------------------------------------------------------------------
!
! PE:   ON INPUT, PE (I) IS THE INDEX IN IW OF THE START OF ROW I, OR
!       ZERO IF ROW I HAS NO OFF-DIAGONAL NON-ZEROS.
!
!       DURING EXECUTION, IT IS USED FOR BOTH SUPERVARIABLES AND
!       ELEMENTS:
!
!       * PRINCIPAL SUPERVARIABLE I:  INDEX INTO IW OF THE
!               DESCRIPTION OF SUPERVARIABLE I.  A SUPERVARIABLE
!               REPRESENTS ONE OR MORE ROWS OF THE MATRIX
!               WITH IDENTICAL NONZERO PATTERN.
!       * NON-PRINCIPAL SUPERVARIABLE I:  IF I HAS BEEN ABSORBED
!               INTO ANOTHER SUPERVARIABLE J, THEN PE (I) = -J.
!               THAT IS, J HAS THE SAME PATTERN AS I.
!               NOTE THAT J MIGHT LATER BE ABSORBED INTO ANOTHER
!               SUPERVARIABLE J2, IN WHICH CASE PE (I) IS STILL -J,
!               AND PE (J) = -J2.
!       * UNABSORBED ELEMENT E:  THE INDEX INTO IW OF THE DESCRIPTION
!               OF ELEMENT E, IF E HAS NOT YET BEEN ABSORBED BY A
!               SUBSEQUENT ELEMENT.  ELEMENT E IS CREATED WHEN
!               THE SUPERVARIABLE OF THE SAME NAME IS SELECTED AS
!               THE PIVOT.
!       * ABSORBED ELEMENT E:  IF ELEMENT E IS ABSORBED INTO ELEMENT
!               E2, THEN PE (E) = -E2.  THIS OCCURS WHEN THE PATTERN OF
!               E (THAT IS, LE) IS FOUND TO BE A SUBSET OF THE PATTERN
!               OF E2 (THAT IS, LE2).  IF ELEMENT E IS "NULL" (IT HAS
!               NO NONZEROS OUTSIDE ITS PIVOT BLOCK), THEN PE (E) = 0.
!
!       ON OUTPUT, PE HOLDS THE ASSEMBLY TREE/FOREST, WHICH IMPLICITLY
!       REPRESENTS A PIVOT ORDER WITH IDENTICAL FILL-IN AS THE ACTUAL
!       ORDER (VIA A DEPTH-FIRST SEARCH OF THE TREE).
!
!       ON OUTPUT:
!       IF NV (I) .GT. 0, THEN I REPRESENTS A NODE IN THE ASSEMBLY TREE,
!       AND THE PARENT OF I IS -PE (I), OR ZERO IF I IS A ROOT.
!       IF NV (I) = 0, THEN (I,-PE (I)) REPRESENTS AN EDGE IN A
!       SUBTREE, THE ROOT OF WHICH IS A NODE IN THE ASSEMBLY TREE.
!
! PFREE:        ON INPUT THE TAIL END OF THE ARRAY, IW (PFREE..IWLEN),
!       IS EMPTY, AND THE MATRIX IS STORED IN IW (1..PFREE-1).
!       DURING EXECUTION, ADDITIONAL DATA IS PLACED IN IW, AND PFREE
!       IS MODIFIED SO THAT IW (PFREE..IWLEN) IS ALWAYS THE UNUSED PART
!       OF IW.  ON OUTPUT, PFREE IS SET EQUAL TO THE SIZE OF IW THAT
!       WOULD HAVE BEEN NEEDED FOR NO COMPRESSIONS TO OCCUR.  IF
!       NCMPA IS ZERO, THEN PFREE (ON OUTPUT) IS LESS THAN OR EQUAL TO
!       IWLEN, AND THE SPACE IW (PFREE+1 ... IWLEN) WAS NOT USED.
!       OTHERWISE, PFREE (ON OUTPUT) IS GREATER THAN IWLEN, AND ALL THE
!       MEMORY IN IW WAS USED.
!
!-----------------------------------------------------------------------
! INPUT/MODIFIED (UNDEFINED ON OUTPUT):
!-----------------------------------------------------------------------
!
! LEN:  ON INPUT, LEN (I) HOLDS THE NUMBER OF ENTRIES IN ROW I OF THE
!       MATRIX, EXCLUDING THE DIAGONAL.  THE CONTENTS OF LEN (1..N)
!       ARE UNDEFINED ON OUTPUT.
!
! IW:   ON INPUT, IW (1..PFREE-1) HOLDS THE DESCRIPTION OF EACH ROW I
!       IN THE MATRIX.  THE MATRIX MUST BE SYMMETRIC, AND BOTH UPPER
!       AND LOWER TRIANGULAR PARTS MUST BE PRESENT.  THE DIAGONAL MUST
!       NOT BE PRESENT.  ROW I IS HELD AS FOLLOWS:
!
!               LEN (I):  THE LENGTH OF THE ROW I DATA STRUCTURE
!               IW (PE (I) ... PE (I) + LEN (I) - 1):
!                       THE LIST OF COLUMN INDICES FOR NONZEROS
!                       IN ROW I (SIMPLE SUPERVARIABLES), EXCLUDING
!                       THE DIAGONAL.  ALL SUPERVARIABLES START WITH
!                       ONE ROW/COLUMN EACH (SUPERVARIABLE I IS JUST
!                       ROW I).
!               IF LEN (I) IS ZERO ON INPUT, THEN PE (I) IS IGNORED
!               ON INPUT.
!
!               NOTE THAT THE ROWS NEED NOT BE IN ANY PARTICULAR ORDER,
!               AND THERE MAY BE EMPTY SPACE BETWEEN THE ROWS.
!
!       DURING EXECUTION, THE SUPERVARIABLE I EXPERIENCES FILL-IN.
!       THIS IS REPRESENTED BY PLACING IN I A LIST OF THE ELEMENTS
!       THAT CAUSE FILL-IN IN SUPERVARIABLE I:
!
!               LEN (I):  THE LENGTH OF SUPERVARIABLE I
!               IW (PE (I) ... PE (I) + ELEN (I) - 1):
!                       THE LIST OF ELEMENTS THAT CONTAIN I.  THIS LIST
!                       IS KEPT SHORT BY REMOVING ABSORBED ELEMENTS.
!               IW (PE (I) + ELEN (I) ... PE (I) + LEN (I) - 1):
!                       THE LIST OF SUPERVARIABLES IN I.  THIS LIST
!                       IS KEPT SHORT BY REMOVING NONPRINCIPAL
!                       VARIABLES, AND ANY ENTRY J THAT IS ALSO
!                       CONTAINED IN AT LEAST ONE OF THE ELEMENTS
!                       (J IN LE) IN THE LIST FOR I (E IN ROW I).
!
!       WHEN SUPERVARIABLE I IS SELECTED AS PIVOT, WE CREATE AN
!       ELEMENT E OF THE SAME NAME (E=I):
!
!               LEN (E):  THE LENGTH OF ELEMENT E
!               IW (PE (E) ... PE (E) + LEN (E) - 1):
!                       THE LIST OF SUPERVARIABLES IN ELEMENT E.
!
!       AN ELEMENT REPRESENTS THE FILL-IN THAT OCCURS WHEN SUPERVARIABLE
!       I IS SELECTED AS PIVOT (WHICH REPRESENTS THE SELECTION OF ROW I
!       AND ALL NON-PRINCIPAL VARIABLES WHOSE PRINCIPAL VARIABLE IS I).
!       WE USE THE TERM LE TO DENOTE THE SET OF ALL SUPERVARIABLES
!       IN ELEMENT E.  ABSORBED SUPERVARIABLES AND ELEMENTS ARE PRUNED
!       FROM THESE LISTS WHEN COMPUTATIONALLY CONVENIENT.
!
!       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
!       THE CONTENTS OF IW ARE UNDEFINED ON OUTPUT.
!
!-----------------------------------------------------------------------
! OUTPUT (NEED NOT BE SET ON INPUT):
!-----------------------------------------------------------------------
!
! NV:   DURING EXECUTION, ABS (NV (I)) IS EQUAL TO THE NUMBER OF ROWS
!       THAT ARE REPRESENTED BY THE PRINCIPAL SUPERVARIABLE I.  IF I IS
!       A NONPRINCIPAL VARIABLE, THEN NV (I) = 0.  INITIALLY,
!       NV (I) = 1 FOR ALL I.  NV (I) .LT. 0 SIGNIFIES THAT I IS A
!       PRINCIPAL VARIABLE IN THE PATTERN LME OF THE CURRENT PIVOT
!       ELEMENT ME.  ON OUTPUT, NV (E) HOLDS THE TRUE DEGREE OF ELEMENT
!       E AT THE TIME IT WAS CREATED (INCLUDING THE DIAGONAL PART).
!
! NCMPA:        THE NUMBER OF TIMES IW WAS COMPRESSED.  IF THIS IS
!       EXCESSIVE, THEN THE EXECUTION TOOK LONGER THAN WHAT COULD HAVE
!       BEEN.  TO REDUCE NCMPA, TRY INCREASING IWLEN TO BE 10% OR 20%
!       LARGER THAN THE VALUE OF PFREE ON INPUT (OR AT LEAST
!       IWLEN .GE. PFREE + N).  THE FASTEST PERFORMANCE WILL BE
!       OBTAINED WHEN NCMPA IS RETURNED AS ZERO.  IF IWLEN IS SET TO
!       THE VALUE RETURNED BY PFREE ON *OUTPUT*, THEN NO COMPRESSIONS
!       WILL OCCUR.
!
! ELEN: SEE THE DESCRIPTION OF IW ABOVE.  AT THE START OF EXECUTION,
!       ELEN (I) IS SET TO ZERO.  DURING EXECUTION, ELEN (I) IS THE
!       NUMBER OF ELEMENTS IN THE LIST FOR SUPERVARIABLE I.  WHEN E
!       BECOMES AN ELEMENT, ELEN (E) = -NEL IS SET, WHERE NEL IS THE
!       CURRENT STEP OF FACTORIZATION.  ELEN (I) = 0 IS DONE WHEN I
!       BECOMES NONPRINCIPAL.
!
!       FOR VARIABLES, ELEN (I) .GE. 0 HOLDS UNTIL JUST BEFORE THE
!       PERMUTATION VECTORS ARE COMPUTED.  FOR ELEMENTS,
!       ELEN (E) .LT. 0 HOLDS.
!
!       ON OUTPUT ELEN (1..N) HOLDS THE INVERSE PERMUTATION (THE SAME
!       AS THE 'INVP' ARGUMENT IN SPARSPAK).  THAT IS, IF K = ELEN (I),
!       THEN ROW I IS THE KTH PIVOT ROW.  ROW I OF A APPEARS AS THE
!       (ELEN(I))-TH ROW IN THE PERMUTED MATRIX, PAPT.
!
! LAST: IN A DEGREE LIST, LAST (I) IS THE SUPERVARIABLE PRECEDING I,
!       OR ZERO IF I IS THE HEAD OF THE LIST.  IN A HASH BUCKET,
!       LAST (I) IS THE HASH KEY FOR I.  LAST (HEAD (HASH)) IS ALSO
!       USED AS THE HEAD OF A HASH BUCKET IF HEAD (HASH) CONTAINS A
!       DEGREE LIST (SEE HEAD, BELOW).
!
!       ON OUTPUT, LAST (1..N) HOLDS THE PERMUTATION (THE SAME AS THE
!       'PERM' ARGUMENT IN SPARSPAK).  THAT IS, IF I = LAST (K), THEN
!       ROW I IS THE KTH PIVOT ROW.  ROW LAST (K) OF A IS THE K-TH ROW
!       IN THE PERMUTED MATRIX, PAPT.
!
!-----------------------------------------------------------------------
! LOCAL (NOT INPUT OR OUTPUT - USED ONLY DURING EXECUTION):
!-----------------------------------------------------------------------
!
! DEGREE:       IF I IS A SUPERVARIABLE, THEN DEGREE (I) HOLDS THE
!       CURRENT APPROXIMATION OF THE EXTERNAL DEGREE OF ROW I (AN UPPER
!       BOUND).  THE EXTERNAL DEGREE IS THE NUMBER OF NONZEROS IN ROW I,
!       MINUS ABS (NV (I)) (THE DIAGONAL PART).  THE BOUND IS EQUAL TO
!       THE EXTERNAL DEGREE IF ELEN (I) IS LESS THAN OR EQUAL TO TWO.
!
!       WE ALSO USE THE TERM "EXTERNAL DEGREE" FOR ELEMENTS E TO REFER
!       TO CARD(LE / LME).  IF E IS AN ELEMENT,
!       THEN DEGREE (E) HOLDS CARD(LE),
!       WHICH IS THE DEGREE OF THE OFF-DIAGONAL PART OF THE ELEMENT E
!       (NOT INCLUDING THE DIAGONAL PART).
!
! HEAD: HEAD IS USED FOR DEGREE LISTS.  HEAD (DEG) IS THE FIRST
!       SUPERVARIABLE IN A DEGREE LIST (ALL SUPERVARIABLES I IN A
!       DEGREE LIST DEG HAVE THE SAME APPROXIMATE DEGREE, NAMELY,
!       DEG = DEGREE (I)).  IF THE LIST DEG IS EMPTY THEN
!       HEAD (DEG) = 0.
!
!       DURING SUPERVARIABLE DETECTION HEAD (HASH) ALSO SERVES AS A
!       POINTER TO A HASH BUCKET.
!       IF HEAD (HASH) .GT. 0, THERE IS A DEGREE LIST OF DEGREE HASH.
!               THE HASH BUCKET HEAD POINTER IS LAST (HEAD (HASH)).
!       IF HEAD (HASH) = 0, THEN THE DEGREE LIST AND HASH BUCKET ARE
!               BOTH EMPTY.
!       IF HEAD (HASH) .LT. 0, THEN THE DEGREE LIST IS EMPTY, AND
!               -HEAD (HASH) IS THE HEAD OF THE HASH BUCKET.
!       AFTER SUPERVARIABLE DETECTION IS COMPLETE, ALL HASH BUCKETS
!       ARE EMPTY, AND THE (LAST (HEAD (HASH)) = 0) CONDITION IS
!       RESTORED FOR THE NON-EMPTY DEGREE LISTS.
!
! NEXT: NEXT (I) IS THE SUPERVARIABLE FOLLOWING I IN A LINK LIST, OR
!       ZERO IF I IS THE LAST IN THE LIST.  USED FOR TWO KINDS OF
!       LISTS:  DEGREE LISTS AND HASH BUCKETS (A SUPERVARIABLE CAN BE
!       IN ONLY ONE KIND OF LIST AT A TIME).
!
! W:    THE FLAG ARRAY W DETERMINES THE STATUS OF ELEMENTS AND
!       VARIABLES, AND THE EXTERNAL DEGREE OF ELEMENTS.
!
!       FOR ELEMENTS:
    integer :: n, iwlen, pfree, ncmpa, iovflo, iw (iwlen), pe (n), degree (n)
    integer :: nv (n), next (n), last (n), head (n), elen (n), w (n), len (n)
!          IF W (E) = 0, THEN THE ELEMENT E IS ABSORBED
!          IF W (E) .GE. WFLG, THEN W (E) - WFLG IS THE SIZE OF
!               THE SET CARD(LE / LME), IN TERMS OF NONZEROS (THE
!               SUM OF ABS (NV (I)) FOR EACH PRINCIPAL VARIABLE I THAT
!               IS BOTH IN THE PATTERN OF ELEMENT E AND NOT IN THE
!               PATTERN OF THE CURRENT PIVOT ELEMENT, ME).
!          IF WFLG .GT. W (E) .GT. 0, THEN E IS NOT ABSORBED AND HAS
!               NOT YET BEEN SEEN IN THE SCAN OF THE ELEMENT LISTS IN
!               THE COMPUTATION OF CARD(LE/LME) IN LOOP 150 BELOW.
!
!       FOR VARIABLES:
!          DURING SUPERVARIABLE DETECTION, IF W (J) .NE. WFLG THEN J IS
!          NOT IN THE PATTERN OF VARIABLE I
!
!       THE W ARRAY IS INITIALIZED BY SETTING W (I) = 1 FOR ALL I,
!       AND BY SETTING WFLG = 2.  IT IS REINITIALIZED IF WFLG BECOMES
!       TOO LARGE (TO ENSURE THAT WFLG+N DOES NOT CAUSE INTEGER
!       OVERFLOW).
!
!-----------------------------------------------------------------------
! LOCAL INTEGERS:
!-----------------------------------------------------------------------
!
    integer :: deg, degme, dmax, e, elenme, eln, hash, hmod, i, ilast, inext, j
    integer :: jlast, jnext, k, knt1, knt2, knt3, lenj, ln, maxmem, me, mem
    integer :: mindeg, nel, newmem, nleft, nvi, nvj, nvpiv, slenme, wbig, we
    integer :: wflg, wnvi, x
!
! DEG:          THE DEGREE OF A VARIABLE OR ELEMENT
! DEGME:        SIZE, CARD(LME), OF THE CURRENT ELEMENT,
!                                            ME (= DEGREE (ME))
! DEXT:         EXTERNAL DEGREE, CARD(LE / LME), OF SOME ELEMENT E
! DMAX:         LARGEST CARD(LE) SEEN SO FAR
! E:            AN ELEMENT
! ELENME:       THE LENGTH, ELEN (ME), OF ELEMENT LIST OF PIVOTAL VAR.
! ELN:          THE LENGTH, ELEN (...), OF AN ELEMENT LIST
! HASH:         THE COMPUTED VALUE OF THE HASH FUNCTION
! HMOD:         THE HASH FUNCTION IS COMPUTED MODULO HMOD = MAX (1,N-1)
! I:            A SUPERVARIABLE
! ILAST:        THE ENTRY IN A LINK LIST PRECEDING I
! INEXT:        THE ENTRY IN A LINK LIST FOLLOWING I
! J:            A SUPERVARIABLE
! JLAST:        THE ENTRY IN A LINK LIST PRECEDING J
! JNEXT:        THE ENTRY IN A LINK LIST, OR PATH, FOLLOWING J
! K:            THE PIVOT ORDER OF AN ELEMENT OR VARIABLE
! KNT1:         LOOP COUNTER USED DURING ELEMENT CONSTRUCTION
! KNT2:         LOOP COUNTER USED DURING ELEMENT CONSTRUCTION
! KNT3:         LOOP COUNTER USED DURING COMPRESSION
! LENJ:         LEN (J)
! LN:           LENGTH OF A SUPERVARIABLE LIST
! MAXMEM:       AMOUNT OF MEMORY NEEDED FOR NO COMPRESSIONS
! ME:           CURRENT SUPERVARIABLE BEING ELIMINATED, AND THE
!                       CURRENT ELEMENT CREATED BY ELIMINATING THAT
!                       SUPERVARIABLE
! MEM:          MEMORY IN USE ASSUMING NO COMPRESSIONS HAVE OCCURRED
! MINDEG:       CURRENT MINIMUM DEGREE
! NEL:          NUMBER OF PIVOTS SELECTED SO FAR
! NEWMEM:       AMOUNT OF NEW MEMORY NEEDED FOR CURRENT PIVOT ELEMENT
! NLEFT:        N - NEL, THE NUMBER OF NONPIVOTAL ROWS/COLUMNS REMAINING
! NVI:          THE NUMBER OF VARIABLES IN A SUPERVARIABLE I (= NV (I))
! NVJ:          THE NUMBER OF VARIABLES IN A SUPERVARIABLE J (= NV (J))
! NVPIV:        NUMBER OF PIVOTS IN CURRENT ELEMENT
! SLENME:       NUMBER OF VARIABLES IN VARIABLE LIST OF PIVOTAL VARIABLE
! WBIG:         = IOVFLO - N.  WFLG IS NOT ALLOWED TO BE .GE. WBIG.
! WE:           W (E)
! WFLG:         USED FOR FLAGGING THE W ARRAY.  SEE DESCRIPTION OF IW.
! WNVI:         WFLG - NV (I)
! X:            EITHER A SUPERVARIABLE OR AN ELEMENT
!
!-----------------------------------------------------------------------
! LOCAL POINTERS:
!-----------------------------------------------------------------------
!
    integer :: p, p1, p2, p3, pdst, pend, pj, pme, pme1, pme2, pn, psrc
!
!               ANY PARAMETER (PE (...) OR PFREE) OR LOCAL VARIABLE
!               STARTING WITH "P" (FOR POINTER) IS AN INDEX INTO IW,
!               AND ALL INDICES INTO IW USE VARIABLES STARTING WITH
!               "P."  THE ONLY EXCEPTION TO THIS RULE IS THE IWLEN
!               INPUT ARGUMENT.
!
! P:            POINTER INTO LOTS OF THINGS
! P1:           PE (I) FOR SOME VARIABLE I (START OF ELEMENT LIST)
! P2:           PE (I) + ELEN (I) -  1 FOR SOME VAR. I (END OF EL. LIST)
! P3:           INDEX OF FIRST SUPERVARIABLE IN CLEAN LIST
! PDST:         DESTINATION POINTER, FOR COMPRESSION
! PEND:         END OF MEMORY TO COMPRESS
! PJ:           POINTER INTO AN ELEMENT OR VARIABLE
! PME:          POINTER INTO THE CURRENT ELEMENT (PME1...PME2)
! PME1:         THE CURRENT ELEMENT, ME, IS STORED IN IW (PME1...PME2)
! PME2:         THE END OF THE CURRENT ELEMENT
! PN:           POINTER INTO A "CLEAN" VARIABLE, ALSO USED TO COMPRESS
! PSRC:         SOURCE POINTER, FOR COMPRESSION
!
!-----------------------------------------------------------------------
!  FUNCTIONS CALLED:
!-----------------------------------------------------------------------
!
!        INTRINSIC MAX, MIN, MOD
!=======================================================================
!  INITIALIZATIONS
!=======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    wflg = 2
    mindeg = 1
    ncmpa = 0
    nel = 0
    hmod = max (1, n-1)
    dmax = 0
    wbig = iovflo - n
    mem = pfree - 1
    maxmem = mem
!
    do 10 i = 1, n
        last (i) = 0
        head (i) = 0
        nv (i) = 1
        w (i) = 1
        elen (i) = 0
        degree (i) = len (i)
10  continue
!
!       ----------------------------------------------------------------
!       INITIALIZE DEGREE LISTS AND ELIMINATE ROWS WITH NO OFF-DIAG. NZ.
!       ----------------------------------------------------------------
!
    do 20 i = 1, n
!
        deg = degree (i)
!
        if (deg .gt. 0) then
!
!             ----------------------------------------------------------
!             PLACE I IN THE DEGREE LIST CORRESPONDING TO ITS DEGREE
!             ----------------------------------------------------------
!
            inext = head (deg)
            if (inext .ne. 0) last (inext) = i
            next (i) = inext
            head (deg) = i
!
        else
!
!             ----------------------------------------------------------
!             WE HAVE A VARIABLE THAT CAN BE ELIMINATED AT ONCE BECAUSE
!             THERE IS NO OFF-DIAGONAL NON-ZERO IN ITS ROW.
!             ----------------------------------------------------------
!
            nel = nel + 1
            elen (i) = -nel
            pe (i) = 0
            w (i) = 0
!
        endif
!
20  continue
!
!=======================================================================
!  WHILE (SELECTING PIVOTS) DO
!=======================================================================
!
30  continue
    if (nel .lt. n) then
!
!=======================================================================
!  GET PIVOT OF MINIMUM DEGREE
!=======================================================================
!
!          -------------------------------------------------------------
!          FIND NEXT SUPERVARIABLE FOR ELIMINATION
!          -------------------------------------------------------------
!
        do 40 deg = mindeg, n
            me = head (deg)
            if (me .gt. 0) goto 50
40      continue
50      continue
        mindeg = deg
!
!          -------------------------------------------------------------
!          REMOVE CHOSEN VARIABLE FROM LINK LIST
!          -------------------------------------------------------------
!
        inext = next (me)
        if (inext .ne. 0) last (inext) = 0
        head (deg) = inext
!
!          -------------------------------------------------------------
!          ME REPRESENTS THE ELIMINATION OF PIVOTS NEL+1 TO NEL+NV(ME).
!          PLACE ME ITSELF AS THE FIRST IN THIS SET.  IT WILL BE MOVED
!          TO THE NEL+NV(ME) POSITION WHEN THE PERMUTATION VECTORS ARE
!          COMPUTED.
!          -------------------------------------------------------------
!
        elenme = elen (me)
        elen (me) = - (nel + 1)
        nvpiv = nv (me)
        nel = nel + nvpiv
!
!=======================================================================
!  CONSTRUCT NEW ELEMENT
!=======================================================================
!
!          -------------------------------------------------------------
!          AT THIS POINT, ME IS THE PIVOTAL SUPERVARIABLE.  IT WILL BE
!          CONVERTED INTO THE CURRENT ELEMENT.  SCAN LIST OF THE
!          PIVOTAL SUPERVARIABLE, ME, SETTING TREE POINTERS AND
!          CONSTRUCTING NEW LIST OF SUPERVARIABLES FOR THE NEW ELEMENT,
!          ME.  P IS A POINTER TO THE CURRENT POSITION IN THE OLD LIST.
!          -------------------------------------------------------------
!
!          FLAG THE VARIABLE "ME" AS BEING IN LME BY NEGATING NV (ME)
        nv (me) = -nvpiv
        degme = 0
!
        if (elenme .eq. 0) then
!
!             ----------------------------------------------------------
!             CONSTRUCT THE NEW ELEMENT IN PLACE
!             ----------------------------------------------------------
!
            pme1 = pe (me)
            pme2 = pme1 - 1
!
            do 60 p = pme1, pme1 + len (me) - 1
                i = iw (p)
                nvi = nv (i)
                if (nvi .gt. 0) then
!
!                   ----------------------------------------------------
!                   I IS A PRINCIPAL VARIABLE NOT YET PLACED IN LME.
!                   STORE I IN NEW LIST
!                   ----------------------------------------------------
!
                    degme = degme + nvi
!                   FLAG I AS BEING IN LME BY NEGATING NV (I)
                    nv (i) = -nvi
                    pme2 = pme2 + 1
                    iw (pme2) = i
!
!                   ----------------------------------------------------
!                   REMOVE VARIABLE I FROM DEGREE LIST.
!                   ----------------------------------------------------
!
                    ilast = last (i)
                    inext = next (i)
                    if (inext .ne. 0) last (inext) = ilast
                    if (ilast .ne. 0) then
                        next (ilast) = inext
                    else
!                      I IS AT THE HEAD OF THE DEGREE LIST
                        head (degree (i)) = inext
                    endif
!
                endif
60          continue
!             THIS ELEMENT TAKES NO NEW MEMORY IN IW:
            newmem = 0
!
        else
!
!             ----------------------------------------------------------
!             CONSTRUCT THE NEW ELEMENT IN EMPTY SPACE, IW (PFREE ...)
!             ----------------------------------------------------------
!
            p = pe (me)
            pme1 = pfree
            slenme = len (me) - elenme
!
            do 120 knt1 = 1, elenme + 1
!
                if (knt1 .gt. elenme) then
!                   SEARCH THE SUPERVARIABLES IN ME.
                    e = me
                    pj = p
                    ln = slenme
                else
!                   SEARCH THE ELEMENTS IN ME.
                    e = iw (p)
                    p = p + 1
                    pj = pe (e)
                    ln = len (e)
                endif
!
!                -------------------------------------------------------
!                SEARCH FOR DIFFERENT SUPERVARIABLES AND ADD THEM TO THE
!                NEW LIST, COMPRESSING WHEN NECESSARY. THIS LOOP IS
!                EXECUTED ONCE FOR EACH ELEMENT IN THE LIST AND ONCE FOR
!                ALL THE SUPERVARIABLES IN THE LIST.
!                -------------------------------------------------------
!
                do 110 knt2 = 1, ln
                    i = iw (pj)
                    pj = pj + 1
                    nvi = nv (i)
                    if (nvi .gt. 0) then
!
!                      -------------------------------------------------
!                      COMPRESS IW, IF NECESSARY
!                      -------------------------------------------------
!
                        if (pfree .gt. iwlen) then
!                         PREPARE FOR COMPRESSING IW BY ADJUSTING
!                         POINTERS AND LENGTHS SO THAT THE LISTS BEING
!                         SEARCHED IN THE INNER AND OUTER LOOPS CONTAIN
!                         ONLY THE REMAINING ENTRIES.
!
                            pe (me) = p
                            len (me) = len (me) - knt1
                            if (len (me) .eq. 0) then
!                            NOTHING LEFT OF SUPERVARIABLE ME
                                pe (me) = 0
                            endif
                            pe (e) = pj
                            len (e) = ln - knt2
                            if (len (e) .eq. 0) then
!                            NOTHING LEFT OF ELEMENT E
                                pe (e) = 0
                            endif
!
                            ncmpa = ncmpa + 1
!                         STORE FIRST ITEM IN PE
!                         SET FIRST ENTRY TO -ITEM
                            do 70 j = 1, n
                                pn = pe (j)
                                if (pn .gt. 0) then
                                    pe (j) = iw (pn)
                                    iw (pn) = -j
                                endif
70                          continue
!
!                         PSRC/PDST POINT TO SOURCE/DESTINATION
                            pdst = 1
                            psrc = 1
                            pend = pme1 - 1
!
!                         WHILE LOOP:
80                          continue
                            if (psrc .le. pend) then
!                            SEARCH FOR NEXT NEGATIVE ENTRY
                                j = -iw (psrc)
                                psrc = psrc + 1
                                if (j .gt. 0) then
                                    iw (pdst) = pe (j)
                                    pe (j) = pdst
                                    pdst = pdst + 1
!                               COPY FROM SOURCE TO DESTINATION
                                    lenj = len (j)
                                    do 90 knt3 = 0, lenj - 2
                                        iw (pdst + knt3) = iw (psrc +&
                                        knt3)
90                                  continue
                                    pdst = pdst + lenj - 1
                                    psrc = psrc + lenj - 1
                                endif
                                goto 80
                            endif
!
!                         MOVE THE NEW PARTIALLY-CONSTRUCTED ELEMENT
                            p1 = pdst
                            do 100 psrc = pme1, pfree - 1
                                iw (pdst) = iw (psrc)
                                pdst = pdst + 1
100                          continue
                            pme1 = p1
                            pfree = pdst
                            pj = pe (e)
                            p = pe (me)
                        endif
!
!                      -------------------------------------------------
!                      I IS A PRINCIPAL VARIABLE NOT YET PLACED IN LME
!                      STORE I IN NEW LIST
!                      -------------------------------------------------
!
                        degme = degme + nvi
!                      FLAG I AS BEING IN LME BY NEGATING NV (I)
                        nv (i) = -nvi
                        iw (pfree) = i
                        pfree = pfree + 1
!
!                      -------------------------------------------------
!                      REMOVE VARIABLE I FROM DEGREE LINK LIST
!                      -------------------------------------------------
!
                        ilast = last (i)
                        inext = next (i)
                        if (inext .ne. 0) last (inext) = ilast
                        if (ilast .ne. 0) then
                            next (ilast) = inext
                        else
!                         I IS AT THE HEAD OF THE DEGREE LIST
                            head (degree (i)) = inext
                        endif
!
                    endif
110              continue
!
                if (e .ne. me) then
!                   SET TREE POINTER AND FLAG TO INDICATE ELEMENT E IS
!                   ABSORBED INTO NEW ELEMENT ME (THE PARENT OF E IS ME)
                    pe (e) = -me
                    w (e) = 0
                endif
120          continue
!
            pme2 = pfree - 1
!             THIS ELEMENT TAKES NEWMEM NEW MEMORY IN IW (POSSIBLY ZERO)
            newmem = pfree - pme1
            mem = mem + newmem
            maxmem = max (maxmem, mem)
        endif
!
!          -------------------------------------------------------------
!          ME HAS NOW BEEN CONVERTED INTO AN ELEMENT IN IW (PME1..PME2)
!          -------------------------------------------------------------
!
!          DEGME HOLDS THE EXTERNAL DEGREE OF NEW ELEMENT
        degree (me) = degme
        pe (me) = pme1
        len (me) = pme2 - pme1 + 1
!
!          -------------------------------------------------------------
!          MAKE SURE THAT WFLG IS NOT TOO LARGE.  WITH THE CURRENT
!          VALUE OF WFLG, WFLG+N MUST NOT CAUSE INTEGER OVERFLOW
!          -------------------------------------------------------------
!
        if (wflg .ge. wbig) then
            do 130 x = 1, n
                if (w (x) .ne. 0) w (x) = 1
130          continue
            wflg = 2
        endif
!
!=======================================================================
!  COMPUTE (W (E) - WFLG) = CARD(LE/LME) FOR ALL ELEMENTS
!=======================================================================
!
!          -------------------------------------------------------------
!          SCAN 1:  COMPUTE THE EXTERNAL DEGREES OF PREVIOUS ELEMENTS
!          WITH RESPECT TO THE CURRENT ELEMENT.  THAT IS:
!               (W (E) - WFLG) = CARD(LE / LME)
!          FOR EACH ELEMENT E THAT APPEARS IN ANY SUPERVARIABLE IN LME.
!          THE NOTATION LE REFERS TO THE PATTERN (LIST OF
!          SUPERVARIABLES) OF A PREVIOUS ELEMENT E, WHERE E IS NOT YET
!          ABSORBED, STORED IN IW (PE (E) + 1 ... PE (E) + IW (PE (E))).
!          THE NOTATION LME REFERS TO THE PATTERN OF THE CURRENT ELEMENT
!          (STORED IN IW (PME1..PME2)).   IF (W (E) - WFLG) BECOMES
!          ZERO, THEN THE ELEMENT E WILL BE ABSORBED IN SCAN 2.
!          -------------------------------------------------------------
!
        do 150 pme = pme1, pme2
            i = iw (pme)
            eln = elen (i)
            if (eln .gt. 0) then
!                NOTE THAT NV (I) HAS BEEN NEGATED TO DENOTE I IN LME:
                nvi = -nv (i)
                wnvi = wflg - nvi
                do 140 p = pe (i), pe (i) + eln - 1
                    e = iw (p)
                    we = w (e)
                    if (we .ge. wflg) then
!                      UNABSORBED ELEMENT E HAS BEEN SEEN IN THIS LOOP
                        we = we - nvi
                    else if (we .ne. 0) then
!                      E IS AN UNABSORBED ELEMENT
!                      THIS IS THE FIRST WE HAVE SEEN E IN ALL OF SCAN 1
                        we = degree (e) + wnvi
                    endif
                    w (e) = we
140              continue
            endif
150      continue
!
!=======================================================================
!  DEGREE UPDATE AND ELEMENT ABSORPTION
!=======================================================================
!
!          -------------------------------------------------------------
!          SCAN 2:  FOR EACH I IN LME, SUM UP THE DEGREE OF LME (WHICH
!          IS DEGME), PLUS THE SUM OF THE EXTERNAL DEGREES OF EACH LE
!          FOR THE ELEMENTS E APPEARING WITHIN I, PLUS THE
!          SUPERVARIABLES IN I.  PLACE I IN HASH LIST.
!          -------------------------------------------------------------
!
        do 180 pme = pme1, pme2
            i = iw (pme)
            p1 = pe (i)
            p2 = p1 + elen (i) - 1
            pn = p1
            hash = 0
            deg = 0
!
!             ----------------------------------------------------------
!             SCAN THE ELEMENT LIST ASSOCIATED WITH SUPERVARIABLE I
!             ----------------------------------------------------------
!
!             UMFPACK/MA38-STYLE APPROXIMATE DEGREE:
            do 160 p = p1, p2
                e = iw (p)
                we = w (e)
                if (we .ne. 0) then
!                   E IS AN UNABSORBED ELEMENT
                    deg = deg + we - wflg
                    iw (pn) = e
                    pn = pn + 1
                    hash = hash + e
                endif
160          continue
!
!             COUNT THE NUMBER OF ELEMENTS IN I (INCLUDING ME):
            elen (i) = pn - p1 + 1
!
!             ----------------------------------------------------------
!             SCAN THE SUPERVARIABLES IN THE LIST ASSOCIATED WITH I
!             ----------------------------------------------------------
!
            p3 = pn
            do 170 p = p2 + 1, p1 + len (i) - 1
                j = iw (p)
                nvj = nv (j)
                if (nvj .gt. 0) then
!                   J IS UNABSORBED, AND NOT IN LME.
!                   ADD TO DEGREE AND ADD TO NEW LIST
                    deg = deg + nvj
                    iw (pn) = j
                    pn = pn + 1
                    hash = hash + j
                endif
170          continue
!
!             ----------------------------------------------------------
!             UPDATE THE DEGREE AND CHECK FOR MASS ELIMINATION
!             ----------------------------------------------------------
!
            if (elen (i) .eq. 1 .and. p3 .eq. pn) then
!
!                -------------------------------------------------------
!                MASS ELIMINATION
!                -------------------------------------------------------
!
!                THERE IS NOTHING LEFT OF THIS NODE EXCEPT FOR AN
!                EDGE TO THE CURRENT PIVOT ELEMENT.  ELEN (I) IS 1,
!                AND THERE ARE NO VARIABLES ADJACENT TO NODE I.
!                ABSORB I INTO THE CURRENT PIVOT ELEMENT, ME.
!
                pe (i) = -me
                nvi = -nv (i)
                degme = degme - nvi
                nvpiv = nvpiv + nvi
                nel = nel + nvi
                nv (i) = 0
                elen (i) = 0
!
            else
!
!                -------------------------------------------------------
!                UPDATE THE UPPER-BOUND DEGREE OF I
!                -------------------------------------------------------
!
!                THE FOLLOWING DEGREE DOES NOT YET INCLUDE THE SIZE
!                OF THE CURRENT ELEMENT, WHICH IS ADDED LATER:
                degree (i) = min (degree (i), deg)
!
!                -------------------------------------------------------
!                ADD ME TO THE LIST FOR I
!                -------------------------------------------------------
!
!                MOVE FIRST SUPERVARIABLE TO END OF LIST
                iw (pn) = iw (p3)
!                MOVE FIRST ELEMENT TO END OF ELEMENT PART OF LIST
                iw (p3) = iw (p1)
!                ADD NEW ELEMENT TO FRONT OF LIST.
                iw (p1) = me
!                STORE THE NEW LENGTH OF THE LIST IN LEN (I)
                len (i) = pn - p1 + 1
!
!                -------------------------------------------------------
!                PLACE IN HASH BUCKET.  SAVE HASH KEY OF I IN LAST (I).
!                -------------------------------------------------------
!
                hash = mod (hash, hmod) + 1
                j = head (hash)
                if (j .le. 0) then
!                   THE DEGREE LIST IS EMPTY, HASH HEAD IS -J
                    next (i) = -j
                    head (hash) = -i
                else
!                   DEGREE LIST IS NOT EMPTY
!                   USE LAST (HEAD (HASH)) AS HASH HEAD
                    next (i) = last (j)
                    last (j) = i
                endif
                last (i) = hash
            endif
180      continue
!
        degree (me) = degme
!
!          -------------------------------------------------------------
!          CLEAR THE COUNTER ARRAY, W (...), BY INCREMENTING WFLG.
!          -------------------------------------------------------------
!
        dmax = max (dmax, degme)
        wflg = wflg + dmax
!
!          MAKE SURE THAT WFLG+N DOES NOT CAUSE INTEGER OVERFLOW
        if (wflg .ge. wbig) then
            do 190 x = 1, n
                if (w (x) .ne. 0) w (x) = 1
190          continue
            wflg = 2
        endif
!          AT THIS POINT, W (1..N) .LT. WFLG HOLDS
!
!=======================================================================
!  SUPERVARIABLE DETECTION
!=======================================================================
!
        do 250 pme = pme1, pme2
            i = iw (pme)
            if (nv (i) .lt. 0) then
!                I IS A PRINCIPAL VARIABLE IN LME
!
!                -------------------------------------------------------
!                EXAMINE ALL HASH BUCKETS WITH 2 OR MORE VARIABLES.  WE
!                DO THIS BY EXAMING ALL UNIQUE HASH KEYS FOR SUPER-
!                VARIABLES IN THE PATTERN LME OF THE CURRENT ELEMENT, ME
!                -------------------------------------------------------
!
                hash = last (i)
!                LET I = HEAD OF HASH BUCKET, AND EMPTY THE HASH BUCKET
                j = head (hash)
                if (j .eq. 0) goto 250
                if (j .lt. 0) then
!                   DEGREE LIST IS EMPTY
                    i = -j
                    head (hash) = 0
                else
!                   DEGREE LIST IS NOT EMPTY, RESTORE LAST () OF HEAD
                    i = last (j)
                    last (j) = 0
                endif
                if (i .eq. 0) goto 250
!
!                WHILE LOOP:
200              continue
                if (next (i) .ne. 0) then
!
!                   ----------------------------------------------------
!                   THIS BUCKET HAS ONE OR MORE VARIABLES FOLLOWING I.
!                   SCAN ALL OF THEM TO SEE IF I CAN ABSORB ANY ENTRIES
!                   THAT FOLLOW I IN HASH BUCKET.  SCATTER I INTO W.
!                   ----------------------------------------------------
!
                    ln = len (i)
                    eln = elen (i)
!                   DO NOT FLAG THE FIRST ELEMENT IN THE LIST (ME)
                    do 210 p = pe (i) + 1, pe (i) + ln - 1
                        w (iw (p)) = wflg
210                  continue
!
!                   ----------------------------------------------------
!                   SCAN EVERY OTHER ENTRY J FOLLOWING I IN BUCKET
!                   ----------------------------------------------------
!
                    jlast = i
                    j = next (i)
!
!                   WHILE LOOP:
220                  continue
                    if (j .ne. 0) then
!
!                      -------------------------------------------------
!                      CHECK IF J AND I HAVE IDENTICAL NONZERO PATTERN
!                      -------------------------------------------------
!
                        if (len (j) .ne. ln) then
!                         I AND J DO NOT HAVE SAME SIZE DATA STRUCTURE
                            goto 240
                        endif
                        if (elen (j) .ne. eln) then
!                         I AND J DO NOT HAVE SAME NUMBER OF ADJACENT EL
                            goto 240
                        endif
!                      DO NOT FLAG THE FIRST ELEMENT IN THE LIST (ME)
                        do 230 p = pe (j) + 1, pe (j) + ln - 1
                            if (w (iw (p)) .ne. wflg) then
!                            AN ENTRY (IW(P)) IS IN J BUT NOT IN I
                                goto 240
                            endif
230                      continue
!
!                      -------------------------------------------------
!                      FOUND IT!  J CAN BE ABSORBED INTO I
!                      -------------------------------------------------
!
                        pe (j) = -i
!                      BOTH NV (I) AND NV (J) ARE NEGATED SINCE THEY
!                      ARE IN LME, AND THE ABSOLUTE VALUES OF EACH
!                      ARE THE NUMBER OF VARIABLES IN I AND J:
                        nv (i) = nv (i) + nv (j)
                        nv (j) = 0
                        elen (j) = 0
!                      DELETE J FROM HASH BUCKET
                        j = next (j)
                        next (jlast) = j
                        goto 220
!
!                      -------------------------------------------------
240                      continue
!                      J CANNOT BE ABSORBED INTO I
!                      -------------------------------------------------
!
                        jlast = j
                        j = next (j)
                        goto 220
                    endif
!
!                   ----------------------------------------------------
!                   NO MORE VARIABLES CAN BE ABSORBED INTO I
!                   GOTO NEXT I IN BUCKET AND CLEAR FLAG ARRAY
!                   ----------------------------------------------------
!
                    wflg = wflg + 1
                    i = next (i)
                    if (i .ne. 0) goto 200
                endif
            endif
250      continue
!
!=======================================================================
!  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
!=======================================================================
!
        p = pme1
        nleft = n - nel
        do 260 pme = pme1, pme2
            i = iw (pme)
            nvi = -nv (i)
            if (nvi .gt. 0) then
!                I IS A PRINCIPAL VARIABLE IN LME
!                RESTORE NV (I) TO SIGNIFY THAT I IS PRINCIPAL
                nv (i) = nvi
!
!                -------------------------------------------------------
!                COMPUTE THE EXTERNAL DEGREE (ADD SIZE OF CURRENT ELEM)
!                -------------------------------------------------------
!
                deg = max (1, min (degree (i) + degme-nvi, nleft-nvi))
!
!                -------------------------------------------------------
!                PLACE THE SUPERVARIABLE AT THE HEAD OF THE DEGREE LIST
!                -------------------------------------------------------
!
                inext = head (deg)
                if (inext .ne. 0) last (inext) = i
                next (i) = inext
                last (i) = 0
                head (deg) = i
!
!                -------------------------------------------------------
!                SAVE THE NEW DEGREE, AND FIND THE MINIMUM DEGREE
!                -------------------------------------------------------
!
                mindeg = min (mindeg, deg)
                degree (i) = deg
!
!                -------------------------------------------------------
!                PLACE THE SUPERVARIABLE IN THE ELEMENT PATTERN
!                -------------------------------------------------------
!
                iw (p) = i
                p = p + 1
            endif
260      continue
!
!=======================================================================
!  FINALIZE THE NEW ELEMENT
!=======================================================================
!
        nv (me) = nvpiv + degme
!          NV (ME) IS NOW THE DEGREE OF PIVOT (INCLUDING DIAGONAL PART)
!          SAVE THE LENGTH OF THE LIST FOR THE NEW ELEMENT ME
        len (me) = p - pme1
        if (len (me) .eq. 0) then
!             THERE IS NOTHING LEFT OF THE CURRENT PIVOT ELEMENT
            pe (me) = 0
            w (me) = 0
        endif
        if (newmem .ne. 0) then
!             ELEMENT WAS NOT CONSTRUCTED IN PLACE: DEALLOCATE PART
!             OF IT (FINAL SIZE IS LESS THAN OR EQUAL TO NEWMEM,
!             SINCE NEWLY NONPRINCIPAL VARIABLES HAVE BEEN REMOVED).
            pfree = p
            mem = mem - newmem + len (me)
        endif
!
!=======================================================================
!          END WHILE (SELECTING PIVOTS)
        goto 30
    endif
!=======================================================================
!
!=======================================================================
!  COMPUTE THE PERMUTATION VECTORS
!=======================================================================
!
!       ----------------------------------------------------------------
!       THE TIME TAKEN BY THE FOLLOWING CODE IS O(N).  AT THIS
!       POINT, ELEN (E) = -K HAS BEEN DONE FOR ALL ELEMENTS E,
!       AND ELEN (I) = 0 HAS BEEN DONE FOR ALL NONPRINCIPAL
!       VARIABLES I.  AT THIS POINT, THERE ARE NO PRINCIPAL
!       SUPERVARIABLES LEFT, AND ALL ELEMENTS ARE ABSORBED.
!       ----------------------------------------------------------------
!
!       ----------------------------------------------------------------
!       COMPUTE THE ORDERING OF UNORDERED NONPRINCIPAL VARIABLES
!       ----------------------------------------------------------------
!
!
    do 290 i = 1, n
        if (elen (i) .eq. 0) then
!
!             ----------------------------------------------------------
!             I IS AN UN-ORDERED ROW.  TRAVERSE THE TREE FROM I UNTIL
!             REACHING AN ELEMENT, E.  THE ELEMENT, E, WAS THE
!             PRINCIPAL SUPERVARIABLE OF I AND ALL NODES IN THE PATH
!             FROM I TO WHEN E WAS SELECTED AS PIVOT.
!             ----------------------------------------------------------
!
            j = -pe (i)
!             WHILE (J IS A VARIABLE) DO:
270          continue
            if (elen (j) .ge. 0) then
                j = -pe (j)
                goto 270
            endif
            e = j
!
!             ----------------------------------------------------------
!             GET THE CURRENT PIVOT ORDERING OF E
!             ----------------------------------------------------------
!
            k = -elen (e)
!
!             ----------------------------------------------------------
!             TRAVERSE THE PATH AGAIN FROM I TO E, AND COMPRESS THE
!             PATH (ALL NODES POINT TO E).  PATH COMPRESSION ALLOWS
!             THIS CODE TO COMPUTE IN O(N) TIME.  ORDER THE UNORDERED
!             NODES IN THE PATH, AND PLACE THE ELEMENT E AT THE END.
!             ----------------------------------------------------------
!
            j = i
!             WHILE (J IS A VARIABLE) DO:
280          continue
            if (elen (j) .ge. 0) then
                jnext = -pe (j)
                pe (j) = -e
                if (elen (j) .eq. 0) then
!                   J IS AN UNORDERED ROW
                    elen (j) = k
                    k = k + 1
                endif
                j = jnext
                goto 280
            endif
!             LEAVE ELEN (E) NEGATIVE, SO WE KNOW IT IS AN ELEMENT
            elen (e) = -k
        endif
290  continue
!
!       ----------------------------------------------------------------
!       RESET THE INVERSE PERMUTATION (ELEN (1..N)) TO BE POSITIVE,
!       AND COMPUTE THE PERMUTATION (LAST (1..N)).
!       ----------------------------------------------------------------
!
    do 300 i = 1, n
        k = abs (elen (i))
        last (k) = i
        elen (i) = k
300  continue
!
!=======================================================================
!  RETURN THE MEMORY USAGE IN IW
!=======================================================================
!
!       IF MAXMEM IS LESS THAN OR EQUAL TO IWLEN, THEN NO COMPRESSIONS
!       OCCURRED, AND IW (MAXMEM+1 ... IWLEN) WAS UNUSED.  OTHERWISE
!       COMPRESSIONS DID OCCUR, AND IWLEN WOULD HAVE HAD TO HAVE BEEN
!       GREATER THAN OR EQUAL TO MAXMEM FOR NO COMPRESSIONS TO OCCUR.
!       RETURN THE VALUE OF MAXMEM IN THE PFREE ARGUMENT.
!
    pfree = maxmem
end subroutine
