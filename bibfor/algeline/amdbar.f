        SUBROUTINE AMDBAR
     >          (N, PE, IW, LEN, IWLEN, PFREE, NV, NEXT,
     >          LAST, HEAD, ELEN, DEGREE, NCMPA, W, IOVFLO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 07/01/2002   AUTEUR JFBHHUC C.ROSE 
C RESPONSABLE JFBHHUC C.ROSE
C TOLE CRP_20
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER N, IWLEN, PFREE, NCMPA, IOVFLO, IW (IWLEN), PE (N),
     >          DEGREE (N), NV (N), NEXT (N), LAST (N), HEAD (N),
     >          ELEN (N), W (N), LEN (N)

C---------------------------------------------------------------
C  THE MC47 / AMD SUITE OF MINIMUM DEGREE ORDERING ALGORITHMS.
C
C  THIS CODE IS ONE OF SEVEN VARIATIONS OF A SINGLE ALGORITHM:
C  THE PRIMARY ROUTINE (MC47B/BD, ONLY AVAILABLE IN THE HARWELL
C  SUBROUTINE LIBRARY), AND 6 VARIATIONS THAT DIFFER ONLY IN
C  HOW THEY COMPUTE THE DEGREE (AVAILABLE IN NETLIB).
C
C  FOR INFORMATION ON THE HARWELL SUBROUTINE LIBRARY, CONTACT
C  MARIA WOODBRIDGE,
C  AEA TECHNOLOGY PRODUCTS AND SYSTEMS
C  HARWELL, DIDCOT, OXFORDSHIRE OX11 0RA,
C  TELEPHONE (44) 1235 432345,
C  FAX       (44) 1235 432023,
C  EMAIL     MARIA.WOODBRIDGE, AT , AEAT.CO.UK
C------------------------------------------------------------------

C********************************************************************
C NOTICE:
C------------------------------------------------------------------
C CADRE GENERAL D'UTILISATION DE LA BIBLIOTHEQUE HARWELL
C------------------------------------------------------------------
C THE AMD ROUTINES (AMDEXA, AMDBAR, AMDHAF, AMDHAT, AMDTRU,
C AND AMDATR) MAY BE USED SOLELY FOR EDUCATIONAL, RESEARCH, AND
C BENCHMARKING PURPOSES BY NON-PROFIT ORGANIZATIONS AND THE U.S.
C GOVERNMENT.  COMMERCIAL AND OTHER ORGANIZATIONS MAY MAKE USE OF THE
C AMD ROUTINES SOLELY FOR BENCHMARKING PURPOSES ONLY.  THE AMD
C ROUTINES MAY BE MODIFIED BY OR ON BEHALF OF THE USER FOR SUCH
C USE BUT AT NO TIME SHALL THE AMD ROUTINES OR ANY SUCH MODIFIED
C VERSION OF THEM BECOME THE PROPERTY OF THE USER.  THE AMD ROUTINES
C ARE PROVIDED WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR
C IMPLIED.  NEITHER THE AUTHORS NOR THEIR EMPLOYERS SHALL BE LIABLE
C FOR ANY DIRECT OR CONSEQUENTIAL LOSS OR DAMAGE WHATSOEVER ARISING
C OUT OF THE USE OR MISUSE OF THE AMD ROUTINES BY THE USER.  THE AMD
C ROUTINES MUST NOT BE SOLD.  YOU MAY MAKE COPIES OF THE AMD ROUTINES,
C BUT THIS NOTICE AND THE COPYRIGHT NOTICE MUST APPEAR IN ALL COPIES.
C ANY OTHER USE OF THE AMD ROUTINES REQUIRES WRITTEN PERMISSION.
C YOUR USE OF THE AMD ROUTINES IS AN IMPLICIT AGREEMENT TO THESE
C CONDITIONS.
C------------------------------------------------------------------
C UTILISATION POUR DES PRESTATIONS DE LA DIVISION R&D
C------------------------------------------------------------------
C EDF - POLE INDUSTRIE - DIVISION RECHERCHE ET DEVELOPPEMENT
C SERVICE ER - DEPARTEMENT MOS - (ARNAUD RENAUD)
C A ACQUIS EN 1997 UNE LICENCE POUR LE SITE DE CLAMART
C (SITE MEANS PREMISES LOCATED AT ONE POSTAL ADDRESS)
C DE L'ENSEMBLE DE LA LIBRAIRIE HARWELL (VERSION 12).
C C'EST UNE "STANDARD" LICENCE (PAS SEULEMENT "ACADEMIC")
C UTILISABLE A DES FINS COMMERCIALES PAR LES UNITES DE CE SITE.
C------------------------------------------------------------------
C UTILISATION POUR DIFFUSION DE LOGICIEL
C------------------------------------------------------------------
C EDF - POLE INDUSTRIE - DIVISION RECHERCHE ET DEVELOPPEMENT
C SERVICE IMA - DEPARTEMENT MMN - (PASCAL ESPOSITO)
C A ACQUIS EN MARS 1999 UNE LICENCE DE LA ROUTINE MC47 DE
C LA BIBLIOTHEQUE HARWELL (VERSION 12, DECEMBRE 1995) :
C 1- POUR UTILISATION DANS LE PROGICIEL CODE_ASTER
C 2- POUR DIFFUSION A DES TIERCES PARTIES DU PROGICIEL INCLUANT CETTE
C    ROUTINE SOUS FORME DE CODE OBJET
C CETTE LICENCE EST PERMANENTE (SANS LIMITATION DE DATE), POUR TOUTE
C PLATE-FORME INFORMATIQUE, EN TOUT LIEU.
C
C ********************************************************************
C-----------------------------------------------------------------------
C AMDBAR:  APPROXIMATE MINIMUM (UMFPACK/MA38-STYLE, EXTERNAL) DEGREE
C          ORDERING ALGORITHM, BUT WITHOUT AGGRESIVE ABSORPTION
C-----------------------------------------------------------------------

C  VARIATION 2:  MC47-STYLE APPROXIMATE EXTERNAL DEGREE, BUT WITH NO
C  AGGRESIVE ABSORPTION.  THIS IS INCLUDED FOR COMPARISON WITH THE
C  OTHER 5 VARIATIONS.  IT TENDS TO COMPUTE ORDERINGS COMPARABLE TO
C  MC47B/BD, OR SLIGHTLY WORSE IN SOME CASES.  IT TENDS TO BE ABOUT AS
C  FAST AS MC47B/BD.
C
C  WE RECOMMEND USING MC47B/BD INSTEAD OF THIS ROUTINE SINCE MC47B/BD
C  GIVES BETTER RESULTS IN ABOUT THE SAME TIME.

C-----------------------------------------------------------------------

C GIVEN A REPRESENTATION OF THE NONZERO PATTERN OF A SYMMETRIC MATRIX,
C       A, (EXCLUDING THE DIAGONAL) PERFORM AN APPROXIMATE MINIMUM
C       (UMFPACK/MA38-STYLE) DEGREE ORDERING TO COMPUTE A PIVOT ORDER
C       SUCH THAT THE INTRODUCTION OF NONZEROS (FILL-IN) IN THE CHOLESKY
C       FACTORS A = LLT ARE KEPT LOW.  AT EACH STEP, THE PIVOT
C       SELECTED IS THE ONE WITH THE MINIMUM UMFAPACK/MA38-STYLE
C       UPPER-BOUND ON THE EXTERNAL DEGREE.  THIS ROUTINE DOES NOT
C       PERFORM AGGRESIVE ABSORPTION (AS DONE BY MC47B/BD).  AGGRESIVE
C       ABSORPTION IN MC47B/BD IS USED TO TIGHTEN THE BOUND ON THE
C       DEGREE.  THIS CAN RESULT AN SIGNIFICANT IMPROVEMENT IN THE
C       QUALITY OF THE ORDERING FOR SOME MATRICES.
C
C       THE APPROXIMATE DEGREE ALGORITHM IMPLEMENTED HERE IS THE
C       SYMMETRIC ANALOG OF THE DEGREE UPDATE ALGORITHM IN MA38 AND
C       UMFPACK (THE UNSYMMETRIC-PATTERN MULTIFRONTAL PACKAGE, BOTH BY
C       DAVIS AND DUFF, AVAILABLE FOR ACADEMIC USERS IN NETLIB AS
C       LINALG/UMFPACK.SHAR OR VIA ANONYMOUS FTP TO
C       FTP.CIS.UFL.EDU:PUB/UMFPACK).  NON-ACADEMIC USERS MUST USE
C       MA38 IN THE HARWELL SUBROUTINE LIBRARY INSTEAD OF UMPFACK.

C **********************************************************************
C ***** CAUTION:  ARGUMENTS ARE NOT CHECKED FOR ERRORS ON INPUT.  ******
C **********************************************************************
C ** IF YOU WANT ERROR CHECKING, A MORE VERSATILE INPUT FORMAT, AND A **
C ** SIMPLER USER INTERFACE, THEN USE MC47A/AD IN THE HARWELL         **
C ** SUBROUTINE LIBRARY, WHICH CHECKS FOR ERRORS, TRANSFORMS THE      **
C ** INPUT, AND CALLS MC47B/BD.                                       **
C **********************************************************************

C       REFERENCES:  (UF TECH REPORTS ARE AVAILABLE VIA ANONYMOUS FTP
C       TO FTP.CIS.UFL.EDU:CIS/TECH-REPORTS).
C
C       (1) TIMOTHY A. DAVIS AND IAIN DUFF, AN UNSYMMETRIC-PATTERN
C               MULTIFRONTAL METHOD FOR SPARSE LU FACTORIZATION,
C               SIAM J. MATRIX ANALYSIS AND APPLICATIONS, TO APPEAR.
C               ALSO UNIV. OF FLORIDA TECHNICAL REPORT TR-94-038.
C               DISCUSSES UMFPACK / MA38.
C
C       (2) PATRICK AMESTOY, TIMOTHY A. DAVIS, AND IAIN S. DUFF,
C               AN APPROXIMATE MINIMUM DEGREE ORDERING ALGORITHM,
C               SIAM J. MATRIX ANALYSIS AND APPLICATIONS (TO APPEAR),
C               ALSO UNIV. OF FLORIDA TECHNICAL REPORT TR-94-039.
C               DISCUSSES THIS ROUTINE.
C
C       (3) ALAN GEORGE AND JOSEPH LIU, THE EVOLUTION OF THE
C               MINIMUM DEGREE ORDERING ALGORITHM, SIAM REVIEW, VOL.
C               31, NO. 1, PP. 1-19, MARCH 1989.  WE LIST BELOW THE
C               FEATURES MENTIONED IN THAT PAPER THAT THIS CODE
C               INCLUDES:
C
C       MASS ELIMINATION:
C               YES.  MA27 RELIED ON SUPERVARIABLE DETECTION FOR MASS
C               ELIMINATION.
C       INDISTINGUISHABLE NODES:
C               YES (WE CALL THESE "SUPERVARIABLES").  THIS WAS ALSO IN
C               THE MA27 CODE - ALTHOUGH WE MODIFIED THE METHOD OF
C               DETECTING THEM (THE PREVIOUS HASH WAS THE TRUE DEGREE,
C               WHICH WE NO LONGER KEEP TRACK OF).  A SUPERVARIABLE IS
C               A SET OF ROWS WITH IDENTICAL NONZERO PATTERN.  ALL
C               VARIABLES IN A SUPERVARIABLE ARE ELIMINATED TOGETHER.
C               EACH SUPERVARIABLE HAS AS ITS NUMERICAL NAME THAT OF
C               ONE OF ITS VARIABLES (ITS PRINCIPAL VARIABLE).
C       QUOTIENT GRAPH REPRESENTATION:
C               YES.  WE USE THE TERM "ELEMENT" FOR THE CLIQUES FORMED
C               DURING ELIMINATION.  THIS WAS ALSO IN THE MA27 CODE.
C               THE ALGORITHM CAN OPERATE IN PLACE, BUT IT WILL WORK
C               MORE EFFICIENTLY IF GIVEN SOME "ELBOW ROOM."
C       ELEMENT ABSORPTION:
C               YES.  THIS WAS ALSO IN THE MA27 CODE.
C       EXTERNAL DEGREE:
C               YES.  THE MA27 CODE WAS BASED ON THE TRUE DEGREE.
C       INCOMPLETE DEGREE UPDATE AND MULTIPLE ELIMINATION:
C               NO.  THIS WAS NOT IN MA27, EITHER.  OUR METHOD OF
C               DEGREE UPDATE WITHIN MC47B/BD IS ELEMENT-BASED, NOT
C               VARIABLE-BASED.  IT IS THUS NOT WELL-SUITED FOR USE
C               WITH INCOMPLETE DEGREE UPDATE OR MULTIPLE ELIMINATION.

C-----------------------------------------------------------------------
C AUTHORS, AND COPYRIGHT (C) 1995 BY:
C       TIMOTHY A. DAVIS, PATRICK AMESTOY, IAIN S. DUFF, & JOHN K. REID.
C
C ACKNOWLEDGEMENTS:
C       THIS WORK (AND THE UMFPACK PACKAGE) WAS SUPPORTED BY THE
C       NATIONAL SCIENCE FOUNDATION (ASC-9111263 AND DMS-9223088).
C       THE UMFPACK/MA38 APPROXIMATE DEGREE UPDATE ALGORITHM, THE
C       UNSYMMETRIC ANALOG WHICH FORMS THE BASIS OF MC47B/BD, WAS
C       DEVELOPED WHILE TIM DAVIS WAS SUPPORTED BY CERFACS (TOULOUSE,
C       FRANCE) IN A POST-DOCTORAL POSITION.
C
C DATE:  SEPTEMBER, 1995
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C INPUT ARGUMENTS (UNALTERED):
C-----------------------------------------------------------------------

C N:    THE MATRIX ORDER.
C
C       RESTRICTION:  1 .LE. N .LT. (IOVFLO/2)-2

C IWLEN:        THE LENGTH OF IW (1..IWLEN).  ON INPUT, THE MATRIX IS
C       STORED IN IW (1..PFREE-1).  HOWEVER, IW (1..IWLEN) SHOULD BE
C       SLIGHTLY LARGER THAN WHAT IS REQUIRED TO HOLD THE MATRIX, AT
C       LEAST IWLEN .GE. PFREE + N IS RECOMMENDED.  OTHERWISE,
C       EXCESSIVE COMPRESSIONS WILL TAKE PLACE.
C       *** WE DO NOT RECOMMEND RUNNING THIS ALGORITHM WITH ***
C       ***      IWLEN .LT. PFREE + N.                      ***
C       *** BETTER PERFORMANCE WILL BE OBTAINED IF          ***
C       ***      IWLEN .GE. PFREE + N                       ***
C       *** OR BETTER YET                                   ***
C       ***      IWLEN .GT. 1.2 * PFREE                     ***
C       *** (WHERE PFREE IS ITS VALUE ON INPUT).            ***
C       THE ALGORITHM WILL NOT RUN AT ALL IF IWLEN .LT. PFREE-1.
C
C       RESTRICTION: IWLEN .GE. PFREE-1

C IOVFLO:       THE LARGEST POSITIVE INTEGER THAT YOUR COMPUTER CAN
C       REPRESENT (-IOVFLO SHOULD ALSO BE REPRESENTABLE).  ON A 32-BIT
C       COMPUTER WITH 2'S-COMPLEMENT ARITHMETIC,
C       IOVFLO = (2 PUISSANCE 31)-1 = 2,147,483,648.

C-----------------------------------------------------------------------
C INPUT/OUPUT ARGUMENTS:
C-----------------------------------------------------------------------

C PE:   ON INPUT, PE (I) IS THE INDEX IN IW OF THE START OF ROW I, OR
C       ZERO IF ROW I HAS NO OFF-DIAGONAL NON-ZEROS.
C
C       DURING EXECUTION, IT IS USED FOR BOTH SUPERVARIABLES AND
C       ELEMENTS:
C
C       * PRINCIPAL SUPERVARIABLE I:  INDEX INTO IW OF THE
C               DESCRIPTION OF SUPERVARIABLE I.  A SUPERVARIABLE
C               REPRESENTS ONE OR MORE ROWS OF THE MATRIX
C               WITH IDENTICAL NONZERO PATTERN.
C       * NON-PRINCIPAL SUPERVARIABLE I:  IF I HAS BEEN ABSORBED
C               INTO ANOTHER SUPERVARIABLE J, THEN PE (I) = -J.
C               THAT IS, J HAS THE SAME PATTERN AS I.
C               NOTE THAT J MIGHT LATER BE ABSORBED INTO ANOTHER
C               SUPERVARIABLE J2, IN WHICH CASE PE (I) IS STILL -J,
C               AND PE (J) = -J2.
C       * UNABSORBED ELEMENT E:  THE INDEX INTO IW OF THE DESCRIPTION
C               OF ELEMENT E, IF E HAS NOT YET BEEN ABSORBED BY A
C               SUBSEQUENT ELEMENT.  ELEMENT E IS CREATED WHEN
C               THE SUPERVARIABLE OF THE SAME NAME IS SELECTED AS
C               THE PIVOT.
C       * ABSORBED ELEMENT E:  IF ELEMENT E IS ABSORBED INTO ELEMENT
C               E2, THEN PE (E) = -E2.  THIS OCCURS WHEN THE PATTERN OF
C               E (THAT IS, LE) IS FOUND TO BE A SUBSET OF THE PATTERN
C               OF E2 (THAT IS, LE2).  IF ELEMENT E IS "NULL" (IT HAS
C               NO NONZEROS OUTSIDE ITS PIVOT BLOCK), THEN PE (E) = 0.
C
C       ON OUTPUT, PE HOLDS THE ASSEMBLY TREE/FOREST, WHICH IMPLICITLY
C       REPRESENTS A PIVOT ORDER WITH IDENTICAL FILL-IN AS THE ACTUAL
C       ORDER (VIA A DEPTH-FIRST SEARCH OF THE TREE).
C
C       ON OUTPUT:
C       IF NV (I) .GT. 0, THEN I REPRESENTS A NODE IN THE ASSEMBLY TREE,
C       AND THE PARENT OF I IS -PE (I), OR ZERO IF I IS A ROOT.
C       IF NV (I) = 0, THEN (I,-PE (I)) REPRESENTS AN EDGE IN A
C       SUBTREE, THE ROOT OF WHICH IS A NODE IN THE ASSEMBLY TREE.

C PFREE:        ON INPUT THE TAIL END OF THE ARRAY, IW (PFREE..IWLEN),
C       IS EMPTY, AND THE MATRIX IS STORED IN IW (1..PFREE-1).
C       DURING EXECUTION, ADDITIONAL DATA IS PLACED IN IW, AND PFREE
C       IS MODIFIED SO THAT IW (PFREE..IWLEN) IS ALWAYS THE UNUSED PART
C       OF IW.  ON OUTPUT, PFREE IS SET EQUAL TO THE SIZE OF IW THAT
C       WOULD HAVE BEEN NEEDED FOR NO COMPRESSIONS TO OCCUR.  IF
C       NCMPA IS ZERO, THEN PFREE (ON OUTPUT) IS LESS THAN OR EQUAL TO
C       IWLEN, AND THE SPACE IW (PFREE+1 ... IWLEN) WAS NOT USED.
C       OTHERWISE, PFREE (ON OUTPUT) IS GREATER THAN IWLEN, AND ALL THE
C       MEMORY IN IW WAS USED.

C-----------------------------------------------------------------------
C INPUT/MODIFIED (UNDEFINED ON OUTPUT):
C-----------------------------------------------------------------------

C LEN:  ON INPUT, LEN (I) HOLDS THE NUMBER OF ENTRIES IN ROW I OF THE
C       MATRIX, EXCLUDING THE DIAGONAL.  THE CONTENTS OF LEN (1..N)
C       ARE UNDEFINED ON OUTPUT.

C IW:   ON INPUT, IW (1..PFREE-1) HOLDS THE DESCRIPTION OF EACH ROW I
C       IN THE MATRIX.  THE MATRIX MUST BE SYMMETRIC, AND BOTH UPPER
C       AND LOWER TRIANGULAR PARTS MUST BE PRESENT.  THE DIAGONAL MUST
C       NOT BE PRESENT.  ROW I IS HELD AS FOLLOWS:
C
C               LEN (I):  THE LENGTH OF THE ROW I DATA STRUCTURE
C               IW (PE (I) ... PE (I) + LEN (I) - 1):
C                       THE LIST OF COLUMN INDICES FOR NONZEROS
C                       IN ROW I (SIMPLE SUPERVARIABLES), EXCLUDING
C                       THE DIAGONAL.  ALL SUPERVARIABLES START WITH
C                       ONE ROW/COLUMN EACH (SUPERVARIABLE I IS JUST
C                       ROW I).
C               IF LEN (I) IS ZERO ON INPUT, THEN PE (I) IS IGNORED
C               ON INPUT.
C
C               NOTE THAT THE ROWS NEED NOT BE IN ANY PARTICULAR ORDER,
C               AND THERE MAY BE EMPTY SPACE BETWEEN THE ROWS.
C
C       DURING EXECUTION, THE SUPERVARIABLE I EXPERIENCES FILL-IN.
C       THIS IS REPRESENTED BY PLACING IN I A LIST OF THE ELEMENTS
C       THAT CAUSE FILL-IN IN SUPERVARIABLE I:
C
C               LEN (I):  THE LENGTH OF SUPERVARIABLE I
C               IW (PE (I) ... PE (I) + ELEN (I) - 1):
C                       THE LIST OF ELEMENTS THAT CONTAIN I.  THIS LIST
C                       IS KEPT SHORT BY REMOVING ABSORBED ELEMENTS.
C               IW (PE (I) + ELEN (I) ... PE (I) + LEN (I) - 1):
C                       THE LIST OF SUPERVARIABLES IN I.  THIS LIST
C                       IS KEPT SHORT BY REMOVING NONPRINCIPAL
C                       VARIABLES, AND ANY ENTRY J THAT IS ALSO
C                       CONTAINED IN AT LEAST ONE OF THE ELEMENTS
C                       (J IN LE) IN THE LIST FOR I (E IN ROW I).
C
C       WHEN SUPERVARIABLE I IS SELECTED AS PIVOT, WE CREATE AN
C       ELEMENT E OF THE SAME NAME (E=I):
C
C               LEN (E):  THE LENGTH OF ELEMENT E
C               IW (PE (E) ... PE (E) + LEN (E) - 1):
C                       THE LIST OF SUPERVARIABLES IN ELEMENT E.
C
C       AN ELEMENT REPRESENTS THE FILL-IN THAT OCCURS WHEN SUPERVARIABLE
C       I IS SELECTED AS PIVOT (WHICH REPRESENTS THE SELECTION OF ROW I
C       AND ALL NON-PRINCIPAL VARIABLES WHOSE PRINCIPAL VARIABLE IS I).
C       WE USE THE TERM LE TO DENOTE THE SET OF ALL SUPERVARIABLES
C       IN ELEMENT E.  ABSORBED SUPERVARIABLES AND ELEMENTS ARE PRUNED
C       FROM THESE LISTS WHEN COMPUTATIONALLY CONVENIENT.
C
C       CAUTION:  THE INPUT MATRIX IS OVERWRITTEN DURING COMPUTATION.
C       THE CONTENTS OF IW ARE UNDEFINED ON OUTPUT.

C-----------------------------------------------------------------------
C OUTPUT (NEED NOT BE SET ON INPUT):
C-----------------------------------------------------------------------

C NV:   DURING EXECUTION, ABS (NV (I)) IS EQUAL TO THE NUMBER OF ROWS
C       THAT ARE REPRESENTED BY THE PRINCIPAL SUPERVARIABLE I.  IF I IS
C       A NONPRINCIPAL VARIABLE, THEN NV (I) = 0.  INITIALLY,
C       NV (I) = 1 FOR ALL I.  NV (I) .LT. 0 SIGNIFIES THAT I IS A
C       PRINCIPAL VARIABLE IN THE PATTERN LME OF THE CURRENT PIVOT
C       ELEMENT ME.  ON OUTPUT, NV (E) HOLDS THE TRUE DEGREE OF ELEMENT
C       E AT THE TIME IT WAS CREATED (INCLUDING THE DIAGONAL PART).

C NCMPA:        THE NUMBER OF TIMES IW WAS COMPRESSED.  IF THIS IS
C       EXCESSIVE, THEN THE EXECUTION TOOK LONGER THAN WHAT COULD HAVE
C       BEEN.  TO REDUCE NCMPA, TRY INCREASING IWLEN TO BE 10% OR 20%
C       LARGER THAN THE VALUE OF PFREE ON INPUT (OR AT LEAST
C       IWLEN .GE. PFREE + N).  THE FASTEST PERFORMANCE WILL BE
C       OBTAINED WHEN NCMPA IS RETURNED AS ZERO.  IF IWLEN IS SET TO
C       THE VALUE RETURNED BY PFREE ON *OUTPUT*, THEN NO COMPRESSIONS
C       WILL OCCUR.

C ELEN: SEE THE DESCRIPTION OF IW ABOVE.  AT THE START OF EXECUTION,
C       ELEN (I) IS SET TO ZERO.  DURING EXECUTION, ELEN (I) IS THE
C       NUMBER OF ELEMENTS IN THE LIST FOR SUPERVARIABLE I.  WHEN E
C       BECOMES AN ELEMENT, ELEN (E) = -NEL IS SET, WHERE NEL IS THE
C       CURRENT STEP OF FACTORIZATION.  ELEN (I) = 0 IS DONE WHEN I
C       BECOMES NONPRINCIPAL.
C
C       FOR VARIABLES, ELEN (I) .GE. 0 HOLDS UNTIL JUST BEFORE THE
C       PERMUTATION VECTORS ARE COMPUTED.  FOR ELEMENTS,
C       ELEN (E) .LT. 0 HOLDS.
C
C       ON OUTPUT ELEN (1..N) HOLDS THE INVERSE PERMUTATION (THE SAME
C       AS THE 'INVP' ARGUMENT IN SPARSPAK).  THAT IS, IF K = ELEN (I),
C       THEN ROW I IS THE KTH PIVOT ROW.  ROW I OF A APPEARS AS THE
C       (ELEN(I))-TH ROW IN THE PERMUTED MATRIX, PAPT.

C LAST: IN A DEGREE LIST, LAST (I) IS THE SUPERVARIABLE PRECEDING I,
C       OR ZERO IF I IS THE HEAD OF THE LIST.  IN A HASH BUCKET,
C       LAST (I) IS THE HASH KEY FOR I.  LAST (HEAD (HASH)) IS ALSO
C       USED AS THE HEAD OF A HASH BUCKET IF HEAD (HASH) CONTAINS A
C       DEGREE LIST (SEE HEAD, BELOW).
C
C       ON OUTPUT, LAST (1..N) HOLDS THE PERMUTATION (THE SAME AS THE
C       'PERM' ARGUMENT IN SPARSPAK).  THAT IS, IF I = LAST (K), THEN
C       ROW I IS THE KTH PIVOT ROW.  ROW LAST (K) OF A IS THE K-TH ROW
C       IN THE PERMUTED MATRIX, PAPT.

C-----------------------------------------------------------------------
C LOCAL (NOT INPUT OR OUTPUT - USED ONLY DURING EXECUTION):
C-----------------------------------------------------------------------

C DEGREE:       IF I IS A SUPERVARIABLE, THEN DEGREE (I) HOLDS THE
C       CURRENT APPROXIMATION OF THE EXTERNAL DEGREE OF ROW I (AN UPPER
C       BOUND).  THE EXTERNAL DEGREE IS THE NUMBER OF NONZEROS IN ROW I,
C       MINUS ABS (NV (I)) (THE DIAGONAL PART).  THE BOUND IS EQUAL TO
C       THE EXTERNAL DEGREE IF ELEN (I) IS LESS THAN OR EQUAL TO TWO.
C
C       WE ALSO USE THE TERM "EXTERNAL DEGREE" FOR ELEMENTS E TO REFER
C       TO CARD(LE / LME).  IF E IS AN ELEMENT,
C       THEN DEGREE (E) HOLDS CARD(LE),
C       WHICH IS THE DEGREE OF THE OFF-DIAGONAL PART OF THE ELEMENT E
C       (NOT INCLUDING THE DIAGONAL PART).

C HEAD: HEAD IS USED FOR DEGREE LISTS.  HEAD (DEG) IS THE FIRST
C       SUPERVARIABLE IN A DEGREE LIST (ALL SUPERVARIABLES I IN A
C       DEGREE LIST DEG HAVE THE SAME APPROXIMATE DEGREE, NAMELY,
C       DEG = DEGREE (I)).  IF THE LIST DEG IS EMPTY THEN
C       HEAD (DEG) = 0.
C
C       DURING SUPERVARIABLE DETECTION HEAD (HASH) ALSO SERVES AS A
C       POINTER TO A HASH BUCKET.
C       IF HEAD (HASH) .GT. 0, THERE IS A DEGREE LIST OF DEGREE HASH.
C               THE HASH BUCKET HEAD POINTER IS LAST (HEAD (HASH)).
C       IF HEAD (HASH) = 0, THEN THE DEGREE LIST AND HASH BUCKET ARE
C               BOTH EMPTY.
C       IF HEAD (HASH) .LT. 0, THEN THE DEGREE LIST IS EMPTY, AND
C               -HEAD (HASH) IS THE HEAD OF THE HASH BUCKET.
C       AFTER SUPERVARIABLE DETECTION IS COMPLETE, ALL HASH BUCKETS
C       ARE EMPTY, AND THE (LAST (HEAD (HASH)) = 0) CONDITION IS
C       RESTORED FOR THE NON-EMPTY DEGREE LISTS.

C NEXT: NEXT (I) IS THE SUPERVARIABLE FOLLOWING I IN A LINK LIST, OR
C       ZERO IF I IS THE LAST IN THE LIST.  USED FOR TWO KINDS OF
C       LISTS:  DEGREE LISTS AND HASH BUCKETS (A SUPERVARIABLE CAN BE
C       IN ONLY ONE KIND OF LIST AT A TIME).

C W:    THE FLAG ARRAY W DETERMINES THE STATUS OF ELEMENTS AND
C       VARIABLES, AND THE EXTERNAL DEGREE OF ELEMENTS.
C
C       FOR ELEMENTS:
C          IF W (E) = 0, THEN THE ELEMENT E IS ABSORBED
C          IF W (E) .GE. WFLG, THEN W (E) - WFLG IS THE SIZE OF
C               THE SET CARD(LE / LME), IN TERMS OF NONZEROS (THE
C               SUM OF ABS (NV (I)) FOR EACH PRINCIPAL VARIABLE I THAT
C               IS BOTH IN THE PATTERN OF ELEMENT E AND NOT IN THE
C               PATTERN OF THE CURRENT PIVOT ELEMENT, ME).
C          IF WFLG .GT. W (E) .GT. 0, THEN E IS NOT ABSORBED AND HAS
C               NOT YET BEEN SEEN IN THE SCAN OF THE ELEMENT LISTS IN
C               THE COMPUTATION OF CARD(LE/LME) IN LOOP 150 BELOW.
C
C       FOR VARIABLES:
C          DURING SUPERVARIABLE DETECTION, IF W (J) .NE. WFLG THEN J IS
C          NOT IN THE PATTERN OF VARIABLE I
C
C       THE W ARRAY IS INITIALIZED BY SETTING W (I) = 1 FOR ALL I,
C       AND BY SETTING WFLG = 2.  IT IS REINITIALIZED IF WFLG BECOMES
C       TOO LARGE (TO ENSURE THAT WFLG+N DOES NOT CAUSE INTEGER
C       OVERFLOW).

C-----------------------------------------------------------------------
C LOCAL INTEGERS:
C-----------------------------------------------------------------------

        INTEGER DEG, DEGME, DEXT, DMAX, E, ELENME, ELN, HASH, HMOD, I,
     >          ILAST, INEXT, J, JLAST, JNEXT, K, KNT1, KNT2, KNT3,
     >          LENJ, LN, MAXMEM, ME, MEM, MINDEG, NEL, NEWMEM,
     >          NLEFT, NVI, NVJ, NVPIV, SLENME, WBIG, WE, WFLG, WNVI, X

C DEG:          THE DEGREE OF A VARIABLE OR ELEMENT
C DEGME:        SIZE, CARD(LME), OF THE CURRENT ELEMENT,
C                                            ME (= DEGREE (ME))
C DEXT:         EXTERNAL DEGREE, CARD(LE / LME), OF SOME ELEMENT E
C DMAX:         LARGEST CARD(LE) SEEN SO FAR
C E:            AN ELEMENT
C ELENME:       THE LENGTH, ELEN (ME), OF ELEMENT LIST OF PIVOTAL VAR.
C ELN:          THE LENGTH, ELEN (...), OF AN ELEMENT LIST
C HASH:         THE COMPUTED VALUE OF THE HASH FUNCTION
C HMOD:         THE HASH FUNCTION IS COMPUTED MODULO HMOD = MAX (1,N-1)
C I:            A SUPERVARIABLE
C ILAST:        THE ENTRY IN A LINK LIST PRECEDING I
C INEXT:        THE ENTRY IN A LINK LIST FOLLOWING I
C J:            A SUPERVARIABLE
C JLAST:        THE ENTRY IN A LINK LIST PRECEDING J
C JNEXT:        THE ENTRY IN A LINK LIST, OR PATH, FOLLOWING J
C K:            THE PIVOT ORDER OF AN ELEMENT OR VARIABLE
C KNT1:         LOOP COUNTER USED DURING ELEMENT CONSTRUCTION
C KNT2:         LOOP COUNTER USED DURING ELEMENT CONSTRUCTION
C KNT3:         LOOP COUNTER USED DURING COMPRESSION
C LENJ:         LEN (J)
C LN:           LENGTH OF A SUPERVARIABLE LIST
C MAXMEM:       AMOUNT OF MEMORY NEEDED FOR NO COMPRESSIONS
C ME:           CURRENT SUPERVARIABLE BEING ELIMINATED, AND THE
C                       CURRENT ELEMENT CREATED BY ELIMINATING THAT
C                       SUPERVARIABLE
C MEM:          MEMORY IN USE ASSUMING NO COMPRESSIONS HAVE OCCURRED
C MINDEG:       CURRENT MINIMUM DEGREE
C NEL:          NUMBER OF PIVOTS SELECTED SO FAR
C NEWMEM:       AMOUNT OF NEW MEMORY NEEDED FOR CURRENT PIVOT ELEMENT
C NLEFT:        N - NEL, THE NUMBER OF NONPIVOTAL ROWS/COLUMNS REMAINING
C NVI:          THE NUMBER OF VARIABLES IN A SUPERVARIABLE I (= NV (I))
C NVJ:          THE NUMBER OF VARIABLES IN A SUPERVARIABLE J (= NV (J))
C NVPIV:        NUMBER OF PIVOTS IN CURRENT ELEMENT
C SLENME:       NUMBER OF VARIABLES IN VARIABLE LIST OF PIVOTAL VARIABLE
C WBIG:         = IOVFLO - N.  WFLG IS NOT ALLOWED TO BE .GE. WBIG.
C WE:           W (E)
C WFLG:         USED FOR FLAGGING THE W ARRAY.  SEE DESCRIPTION OF IW.
C WNVI:         WFLG - NV (I)
C X:            EITHER A SUPERVARIABLE OR AN ELEMENT

C-----------------------------------------------------------------------
C LOCAL POINTERS:
C-----------------------------------------------------------------------

        INTEGER P, P1, P2, P3, PDST, PEND, PJ, PME, PME1, PME2, PN, PSRC

C               ANY PARAMETER (PE (...) OR PFREE) OR LOCAL VARIABLE
C               STARTING WITH "P" (FOR POINTER) IS AN INDEX INTO IW,
C               AND ALL INDICES INTO IW USE VARIABLES STARTING WITH
C               "P."  THE ONLY EXCEPTION TO THIS RULE IS THE IWLEN
C               INPUT ARGUMENT.

C P:            POINTER INTO LOTS OF THINGS
C P1:           PE (I) FOR SOME VARIABLE I (START OF ELEMENT LIST)
C P2:           PE (I) + ELEN (I) -  1 FOR SOME VAR. I (END OF EL. LIST)
C P3:           INDEX OF FIRST SUPERVARIABLE IN CLEAN LIST
C PDST:         DESTINATION POINTER, FOR COMPRESSION
C PEND:         END OF MEMORY TO COMPRESS
C PJ:           POINTER INTO AN ELEMENT OR VARIABLE
C PME:          POINTER INTO THE CURRENT ELEMENT (PME1...PME2)
C PME1:         THE CURRENT ELEMENT, ME, IS STORED IN IW (PME1...PME2)
C PME2:         THE END OF THE CURRENT ELEMENT
C PN:           POINTER INTO A "CLEAN" VARIABLE, ALSO USED TO COMPRESS
C PSRC:         SOURCE POINTER, FOR COMPRESSION

C-----------------------------------------------------------------------
C  FUNCTIONS CALLED:
C-----------------------------------------------------------------------

C        INTRINSIC MAX, MIN, MOD
C=======================================================================
C  INITIALIZATIONS
C=======================================================================

        WFLG = 2
        MINDEG = 1
        NCMPA = 0
        NEL = 0
        HMOD = MAX (1, N-1)
        DMAX = 0
        WBIG = IOVFLO - N
        MEM = PFREE - 1
        MAXMEM = MEM

        DO 10 I = 1, N
           LAST (I) = 0
           HEAD (I) = 0
           NV (I) = 1
           W (I) = 1
           ELEN (I) = 0
           DEGREE (I) = LEN (I)
10         CONTINUE

C       ----------------------------------------------------------------
C       INITIALIZE DEGREE LISTS AND ELIMINATE ROWS WITH NO OFF-DIAG. NZ.
C       ----------------------------------------------------------------

        DO 20 I = 1, N

           DEG = DEGREE (I)

           IF (DEG .GT. 0) THEN

C             ----------------------------------------------------------
C             PLACE I IN THE DEGREE LIST CORRESPONDING TO ITS DEGREE
C             ----------------------------------------------------------

              INEXT = HEAD (DEG)
              IF (INEXT .NE. 0) LAST (INEXT) = I
              NEXT (I) = INEXT
              HEAD (DEG) = I

           ELSE

C             ----------------------------------------------------------
C             WE HAVE A VARIABLE THAT CAN BE ELIMINATED AT ONCE BECAUSE
C             THERE IS NO OFF-DIAGONAL NON-ZERO IN ITS ROW.
C             ----------------------------------------------------------

              NEL = NEL + 1
              ELEN (I) = -NEL
              PE (I) = 0
              W (I) = 0

              ENDIF

20         CONTINUE

C=======================================================================
C  WHILE (SELECTING PIVOTS) DO
C=======================================================================

30      CONTINUE
        IF (NEL .LT. N) THEN

C=======================================================================
C  GET PIVOT OF MINIMUM DEGREE
C=======================================================================

C          -------------------------------------------------------------
C          FIND NEXT SUPERVARIABLE FOR ELIMINATION
C          -------------------------------------------------------------

           DO 40 DEG = MINDEG, N
              ME = HEAD (DEG)
              IF (ME .GT. 0) GOTO 50
40            CONTINUE
50         CONTINUE
           MINDEG = DEG

C          -------------------------------------------------------------
C          REMOVE CHOSEN VARIABLE FROM LINK LIST
C          -------------------------------------------------------------

           INEXT = NEXT (ME)
           IF (INEXT .NE. 0) LAST (INEXT) = 0
           HEAD (DEG) = INEXT

C          -------------------------------------------------------------
C          ME REPRESENTS THE ELIMINATION OF PIVOTS NEL+1 TO NEL+NV(ME).
C          PLACE ME ITSELF AS THE FIRST IN THIS SET.  IT WILL BE MOVED
C          TO THE NEL+NV(ME) POSITION WHEN THE PERMUTATION VECTORS ARE
C          COMPUTED.
C          -------------------------------------------------------------

           ELENME = ELEN (ME)
           ELEN (ME) = - (NEL + 1)
           NVPIV = NV (ME)
           NEL = NEL + NVPIV

C=======================================================================
C  CONSTRUCT NEW ELEMENT
C=======================================================================

C          -------------------------------------------------------------
C          AT THIS POINT, ME IS THE PIVOTAL SUPERVARIABLE.  IT WILL BE
C          CONVERTED INTO THE CURRENT ELEMENT.  SCAN LIST OF THE
C          PIVOTAL SUPERVARIABLE, ME, SETTING TREE POINTERS AND
C          CONSTRUCTING NEW LIST OF SUPERVARIABLES FOR THE NEW ELEMENT,
C          ME.  P IS A POINTER TO THE CURRENT POSITION IN THE OLD LIST.
C          -------------------------------------------------------------

C          FLAG THE VARIABLE "ME" AS BEING IN LME BY NEGATING NV (ME)
           NV (ME) = -NVPIV
           DEGME = 0

           IF (ELENME .EQ. 0) THEN

C             ----------------------------------------------------------
C             CONSTRUCT THE NEW ELEMENT IN PLACE
C             ----------------------------------------------------------

              PME1 = PE (ME)
              PME2 = PME1 - 1

              DO 60 P = PME1, PME1 + LEN (ME) - 1
                 I = IW (P)
                 NVI = NV (I)
                 IF (NVI .GT. 0) THEN

C                   ----------------------------------------------------
C                   I IS A PRINCIPAL VARIABLE NOT YET PLACED IN LME.
C                   STORE I IN NEW LIST
C                   ----------------------------------------------------

                    DEGME = DEGME + NVI
C                   FLAG I AS BEING IN LME BY NEGATING NV (I)
                    NV (I) = -NVI
                    PME2 = PME2 + 1
                    IW (PME2) = I

C                   ----------------------------------------------------
C                   REMOVE VARIABLE I FROM DEGREE LIST.
C                   ----------------------------------------------------

                    ILAST = LAST (I)
                    INEXT = NEXT (I)
                    IF (INEXT .NE. 0) LAST (INEXT) = ILAST
                    IF (ILAST .NE. 0) THEN
                       NEXT (ILAST) = INEXT
                    ELSE
C                      I IS AT THE HEAD OF THE DEGREE LIST
                       HEAD (DEGREE (I)) = INEXT
                       ENDIF

                    ENDIF
60               CONTINUE
C             THIS ELEMENT TAKES NO NEW MEMORY IN IW:
              NEWMEM = 0

           ELSE

C             ----------------------------------------------------------
C             CONSTRUCT THE NEW ELEMENT IN EMPTY SPACE, IW (PFREE ...)
C             ----------------------------------------------------------

              P = PE (ME)
              PME1 = PFREE
              SLENME = LEN (ME) - ELENME

              DO 120 KNT1 = 1, ELENME + 1

                 IF (KNT1 .GT. ELENME) THEN
C                   SEARCH THE SUPERVARIABLES IN ME.
                    E = ME
                    PJ = P
                    LN = SLENME
                 ELSE
C                   SEARCH THE ELEMENTS IN ME.
                    E = IW (P)
                    P = P + 1
                    PJ = PE (E)
                    LN = LEN (E)
                    ENDIF

C                -------------------------------------------------------
C                SEARCH FOR DIFFERENT SUPERVARIABLES AND ADD THEM TO THE
C                NEW LIST, COMPRESSING WHEN NECESSARY. THIS LOOP IS
C                EXECUTED ONCE FOR EACH ELEMENT IN THE LIST AND ONCE FOR
C                ALL THE SUPERVARIABLES IN THE LIST.
C                -------------------------------------------------------

                 DO 110 KNT2 = 1, LN
                    I = IW (PJ)
                    PJ = PJ + 1
                    NVI = NV (I)
                    IF (NVI .GT. 0) THEN

C                      -------------------------------------------------
C                      COMPRESS IW, IF NECESSARY
C                      -------------------------------------------------

                       IF (PFREE .GT. IWLEN) THEN
C                         PREPARE FOR COMPRESSING IW BY ADJUSTING
C                         POINTERS AND LENGTHS SO THAT THE LISTS BEING
C                         SEARCHED IN THE INNER AND OUTER LOOPS CONTAIN
C                         ONLY THE REMAINING ENTRIES.

                          PE (ME) = P
                          LEN (ME) = LEN (ME) - KNT1
                          IF (LEN (ME) .EQ. 0) THEN
C                            NOTHING LEFT OF SUPERVARIABLE ME
                             PE (ME) = 0
                             ENDIF
                          PE (E) = PJ
                          LEN (E) = LN - KNT2
                          IF (LEN (E) .EQ. 0) THEN
C                            NOTHING LEFT OF ELEMENT E
                             PE (E) = 0
                             ENDIF

                          NCMPA = NCMPA + 1
C                         STORE FIRST ITEM IN PE
C                         SET FIRST ENTRY TO -ITEM
                          DO 70 J = 1, N
                             PN = PE (J)
                             IF (PN .GT. 0) THEN
                                PE (J) = IW (PN)
                                IW (PN) = -J
                                ENDIF
70                           CONTINUE

C                         PSRC/PDST POINT TO SOURCE/DESTINATION
                          PDST = 1
                          PSRC = 1
                          PEND = PME1 - 1

C                         WHILE LOOP:
80                        CONTINUE
                          IF (PSRC .LE. PEND) THEN
C                            SEARCH FOR NEXT NEGATIVE ENTRY
                             J = -IW (PSRC)
                             PSRC = PSRC + 1
                             IF (J .GT. 0) THEN
                                IW (PDST) = PE (J)
                                PE (J) = PDST
                                PDST = PDST + 1
C                               COPY FROM SOURCE TO DESTINATION
                                LENJ = LEN (J)
                                DO 90 KNT3 = 0, LENJ - 2
                                   IW (PDST + KNT3) = IW (PSRC + KNT3)
90                                 CONTINUE
                                PDST = PDST + LENJ - 1
                                PSRC = PSRC + LENJ - 1
                                ENDIF
                             GOTO 80
                             ENDIF

C                         MOVE THE NEW PARTIALLY-CONSTRUCTED ELEMENT
                          P1 = PDST
                          DO 100 PSRC = PME1, PFREE - 1
                             IW (PDST) = IW (PSRC)
                             PDST = PDST + 1
100                          CONTINUE
                          PME1 = P1
                          PFREE = PDST
                          PJ = PE (E)
                          P = PE (ME)
                          ENDIF

C                      -------------------------------------------------
C                      I IS A PRINCIPAL VARIABLE NOT YET PLACED IN LME
C                      STORE I IN NEW LIST
C                      -------------------------------------------------

                       DEGME = DEGME + NVI
C                      FLAG I AS BEING IN LME BY NEGATING NV (I)
                       NV (I) = -NVI
                       IW (PFREE) = I
                       PFREE = PFREE + 1

C                      -------------------------------------------------
C                      REMOVE VARIABLE I FROM DEGREE LINK LIST
C                      -------------------------------------------------

                       ILAST = LAST (I)
                       INEXT = NEXT (I)
                       IF (INEXT .NE. 0) LAST (INEXT) = ILAST
                       IF (ILAST .NE. 0) THEN
                          NEXT (ILAST) = INEXT
                       ELSE
C                         I IS AT THE HEAD OF THE DEGREE LIST
                          HEAD (DEGREE (I)) = INEXT
                          ENDIF

                       ENDIF
110                 CONTINUE

                 IF (E .NE. ME) THEN
C                   SET TREE POINTER AND FLAG TO INDICATE ELEMENT E IS
C                   ABSORBED INTO NEW ELEMENT ME (THE PARENT OF E IS ME)
                    PE (E) = -ME
                    W (E) = 0
                    ENDIF
120              CONTINUE

              PME2 = PFREE - 1
C             THIS ELEMENT TAKES NEWMEM NEW MEMORY IN IW (POSSIBLY ZERO)
              NEWMEM = PFREE - PME1
              MEM = MEM + NEWMEM
              MAXMEM = MAX (MAXMEM, MEM)
              ENDIF

C          -------------------------------------------------------------
C          ME HAS NOW BEEN CONVERTED INTO AN ELEMENT IN IW (PME1..PME2)
C          -------------------------------------------------------------

C          DEGME HOLDS THE EXTERNAL DEGREE OF NEW ELEMENT
           DEGREE (ME) = DEGME
           PE (ME) = PME1
           LEN (ME) = PME2 - PME1 + 1

C          -------------------------------------------------------------
C          MAKE SURE THAT WFLG IS NOT TOO LARGE.  WITH THE CURRENT
C          VALUE OF WFLG, WFLG+N MUST NOT CAUSE INTEGER OVERFLOW
C          -------------------------------------------------------------

           IF (WFLG .GE. WBIG) THEN
              DO 130 X = 1, N
                 IF (W (X) .NE. 0) W (X) = 1
130              CONTINUE
              WFLG = 2
              ENDIF

C=======================================================================
C  COMPUTE (W (E) - WFLG) = CARD(LE/LME) FOR ALL ELEMENTS
C=======================================================================

C          -------------------------------------------------------------
C          SCAN 1:  COMPUTE THE EXTERNAL DEGREES OF PREVIOUS ELEMENTS
C          WITH RESPECT TO THE CURRENT ELEMENT.  THAT IS:
C               (W (E) - WFLG) = CARD(LE / LME)
C          FOR EACH ELEMENT E THAT APPEARS IN ANY SUPERVARIABLE IN LME.
C          THE NOTATION LE REFERS TO THE PATTERN (LIST OF
C          SUPERVARIABLES) OF A PREVIOUS ELEMENT E, WHERE E IS NOT YET
C          ABSORBED, STORED IN IW (PE (E) + 1 ... PE (E) + IW (PE (E))).
C          THE NOTATION LME REFERS TO THE PATTERN OF THE CURRENT ELEMENT
C          (STORED IN IW (PME1..PME2)).   IF (W (E) - WFLG) BECOMES
C          ZERO, THEN THE ELEMENT E WILL BE ABSORBED IN SCAN 2.
C          -------------------------------------------------------------

           DO 150 PME = PME1, PME2
              I = IW (PME)
              ELN = ELEN (I)
              IF (ELN .GT. 0) THEN
C                NOTE THAT NV (I) HAS BEEN NEGATED TO DENOTE I IN LME:
                 NVI = -NV (I)
                 WNVI = WFLG - NVI
                 DO 140 P = PE (I), PE (I) + ELN - 1
                    E = IW (P)
                    WE = W (E)
                    IF (WE .GE. WFLG) THEN
C                      UNABSORBED ELEMENT E HAS BEEN SEEN IN THIS LOOP
                       WE = WE - NVI
                    ELSE IF (WE .NE. 0) THEN
C                      E IS AN UNABSORBED ELEMENT
C                      THIS IS THE FIRST WE HAVE SEEN E IN ALL OF SCAN 1
                       WE = DEGREE (E) + WNVI
                       ENDIF
                    W (E) = WE
140                 CONTINUE
                 ENDIF
150           CONTINUE

C=======================================================================
C  DEGREE UPDATE AND ELEMENT ABSORPTION
C=======================================================================

C          -------------------------------------------------------------
C          SCAN 2:  FOR EACH I IN LME, SUM UP THE DEGREE OF LME (WHICH
C          IS DEGME), PLUS THE SUM OF THE EXTERNAL DEGREES OF EACH LE
C          FOR THE ELEMENTS E APPEARING WITHIN I, PLUS THE
C          SUPERVARIABLES IN I.  PLACE I IN HASH LIST.
C          -------------------------------------------------------------

           DO 180 PME = PME1, PME2
              I = IW (PME)
              P1 = PE (I)
              P2 = P1 + ELEN (I) - 1
              PN = P1
              HASH = 0
              DEG = 0

C             ----------------------------------------------------------
C             SCAN THE ELEMENT LIST ASSOCIATED WITH SUPERVARIABLE I
C             ----------------------------------------------------------

C             UMFPACK/MA38-STYLE APPROXIMATE DEGREE:
              DO 160 P = P1, P2
                 E = IW (P)
                 WE = W (E)
                 IF (WE .NE. 0) THEN
C                   E IS AN UNABSORBED ELEMENT
                    DEG = DEG + WE - WFLG
                    IW (PN) = E
                    PN = PN + 1
                    HASH = HASH + E
                    ENDIF
160              CONTINUE

C             COUNT THE NUMBER OF ELEMENTS IN I (INCLUDING ME):
              ELEN (I) = PN - P1 + 1

C             ----------------------------------------------------------
C             SCAN THE SUPERVARIABLES IN THE LIST ASSOCIATED WITH I
C             ----------------------------------------------------------

              P3 = PN
              DO 170 P = P2 + 1, P1 + LEN (I) - 1
                 J = IW (P)
                 NVJ = NV (J)
                 IF (NVJ .GT. 0) THEN
C                   J IS UNABSORBED, AND NOT IN LME.
C                   ADD TO DEGREE AND ADD TO NEW LIST
                    DEG = DEG + NVJ
                    IW (PN) = J
                    PN = PN + 1
                    HASH = HASH + J
                    ENDIF
170              CONTINUE

C             ----------------------------------------------------------
C             UPDATE THE DEGREE AND CHECK FOR MASS ELIMINATION
C             ----------------------------------------------------------

              IF (ELEN (I) .EQ. 1 .AND. P3 .EQ. PN) THEN

C                -------------------------------------------------------
C                MASS ELIMINATION
C                -------------------------------------------------------

C                THERE IS NOTHING LEFT OF THIS NODE EXCEPT FOR AN
C                EDGE TO THE CURRENT PIVOT ELEMENT.  ELEN (I) IS 1,
C                AND THERE ARE NO VARIABLES ADJACENT TO NODE I.
C                ABSORB I INTO THE CURRENT PIVOT ELEMENT, ME.

                 PE (I) = -ME
                 NVI = -NV (I)
                 DEGME = DEGME - NVI
                 NVPIV = NVPIV + NVI
                 NEL = NEL + NVI
                 NV (I) = 0
                 ELEN (I) = 0

              ELSE

C                -------------------------------------------------------
C                UPDATE THE UPPER-BOUND DEGREE OF I
C                -------------------------------------------------------

C                THE FOLLOWING DEGREE DOES NOT YET INCLUDE THE SIZE
C                OF THE CURRENT ELEMENT, WHICH IS ADDED LATER:
                 DEGREE (I) = MIN (DEGREE (I), DEG)

C                -------------------------------------------------------
C                ADD ME TO THE LIST FOR I
C                -------------------------------------------------------

C                MOVE FIRST SUPERVARIABLE TO END OF LIST
                 IW (PN) = IW (P3)
C                MOVE FIRST ELEMENT TO END OF ELEMENT PART OF LIST
                 IW (P3) = IW (P1)
C                ADD NEW ELEMENT TO FRONT OF LIST.
                 IW (P1) = ME
C                STORE THE NEW LENGTH OF THE LIST IN LEN (I)
                 LEN (I) = PN - P1 + 1

C                -------------------------------------------------------
C                PLACE IN HASH BUCKET.  SAVE HASH KEY OF I IN LAST (I).
C                -------------------------------------------------------

                 HASH = MOD (HASH, HMOD) + 1
                 J = HEAD (HASH)
                 IF (J .LE. 0) THEN
C                   THE DEGREE LIST IS EMPTY, HASH HEAD IS -J
                    NEXT (I) = -J
                    HEAD (HASH) = -I
                 ELSE
C                   DEGREE LIST IS NOT EMPTY
C                   USE LAST (HEAD (HASH)) AS HASH HEAD
                    NEXT (I) = LAST (J)
                    LAST (J) = I
                    ENDIF
                 LAST (I) = HASH
                 ENDIF
180           CONTINUE

           DEGREE (ME) = DEGME

C          -------------------------------------------------------------
C          CLEAR THE COUNTER ARRAY, W (...), BY INCREMENTING WFLG.
C          -------------------------------------------------------------

           DMAX = MAX (DMAX, DEGME)
           WFLG = WFLG + DMAX

C          MAKE SURE THAT WFLG+N DOES NOT CAUSE INTEGER OVERFLOW
           IF (WFLG .GE. WBIG) THEN
              DO 190 X = 1, N
                 IF (W (X) .NE. 0) W (X) = 1
190              CONTINUE
              WFLG = 2
              ENDIF
C          AT THIS POINT, W (1..N) .LT. WFLG HOLDS

C=======================================================================
C  SUPERVARIABLE DETECTION
C=======================================================================

           DO 250 PME = PME1, PME2
              I = IW (PME)
              IF (NV (I) .LT. 0) THEN
C                I IS A PRINCIPAL VARIABLE IN LME

C                -------------------------------------------------------
C                EXAMINE ALL HASH BUCKETS WITH 2 OR MORE VARIABLES.  WE
C                DO THIS BY EXAMING ALL UNIQUE HASH KEYS FOR SUPER-
C                VARIABLES IN THE PATTERN LME OF THE CURRENT ELEMENT, ME
C                -------------------------------------------------------

                 HASH = LAST (I)
C                LET I = HEAD OF HASH BUCKET, AND EMPTY THE HASH BUCKET
                 J = HEAD (HASH)
                 IF (J .EQ. 0) GOTO 250
                 IF (J .LT. 0) THEN
C                   DEGREE LIST IS EMPTY
                    I = -J
                    HEAD (HASH) = 0
                 ELSE
C                   DEGREE LIST IS NOT EMPTY, RESTORE LAST () OF HEAD
                    I = LAST (J)
                    LAST (J) = 0
                    ENDIF
                 IF (I .EQ. 0) GOTO 250

C                WHILE LOOP:
200              CONTINUE
                 IF (NEXT (I) .NE. 0) THEN

C                   ----------------------------------------------------
C                   THIS BUCKET HAS ONE OR MORE VARIABLES FOLLOWING I.
C                   SCAN ALL OF THEM TO SEE IF I CAN ABSORB ANY ENTRIES
C                   THAT FOLLOW I IN HASH BUCKET.  SCATTER I INTO W.
C                   ----------------------------------------------------

                    LN = LEN (I)
                    ELN = ELEN (I)
C                   DO NOT FLAG THE FIRST ELEMENT IN THE LIST (ME)
                    DO 210 P = PE (I) + 1, PE (I) + LN - 1
                       W (IW (P)) = WFLG
210                    CONTINUE

C                   ----------------------------------------------------
C                   SCAN EVERY OTHER ENTRY J FOLLOWING I IN BUCKET
C                   ----------------------------------------------------

                    JLAST = I
                    J = NEXT (I)

C                   WHILE LOOP:
220                 CONTINUE
                    IF (J .NE. 0) THEN

C                      -------------------------------------------------
C                      CHECK IF J AND I HAVE IDENTICAL NONZERO PATTERN
C                      -------------------------------------------------

                       IF (LEN (J) .NE. LN) THEN
C                         I AND J DO NOT HAVE SAME SIZE DATA STRUCTURE
                          GOTO 240
                          ENDIF
                       IF (ELEN (J) .NE. ELN) THEN
C                         I AND J DO NOT HAVE SAME NUMBER OF ADJACENT EL
                          GOTO 240
                          ENDIF
C                      DO NOT FLAG THE FIRST ELEMENT IN THE LIST (ME)
                       DO 230 P = PE (J) + 1, PE (J) + LN - 1
                          IF (W (IW (P)) .NE. WFLG) THEN
C                            AN ENTRY (IW(P)) IS IN J BUT NOT IN I
                             GOTO 240
                             ENDIF
230                       CONTINUE

C                      -------------------------------------------------
C                      FOUND IT!  J CAN BE ABSORBED INTO I
C                      -------------------------------------------------

                       PE (J) = -I
C                      BOTH NV (I) AND NV (J) ARE NEGATED SINCE THEY
C                      ARE IN LME, AND THE ABSOLUTE VALUES OF EACH
C                      ARE THE NUMBER OF VARIABLES IN I AND J:
                       NV (I) = NV (I) + NV (J)
                       NV (J) = 0
                       ELEN (J) = 0
C                      DELETE J FROM HASH BUCKET
                       J = NEXT (J)
                       NEXT (JLAST) = J
                       GOTO 220

C                      -------------------------------------------------
240                    CONTINUE
C                      J CANNOT BE ABSORBED INTO I
C                      -------------------------------------------------

                       JLAST = J
                       J = NEXT (J)
                       GOTO 220
                       ENDIF

C                   ----------------------------------------------------
C                   NO MORE VARIABLES CAN BE ABSORBED INTO I
C                   GO TO NEXT I IN BUCKET AND CLEAR FLAG ARRAY
C                   ----------------------------------------------------

                    WFLG = WFLG + 1
                    I = NEXT (I)
                    IF (I .NE. 0) GOTO 200
                    ENDIF
                 ENDIF
250           CONTINUE

C=======================================================================
C  RESTORE DEGREE LISTS AND REMOVE NONPRINCIPAL SUPERVAR. FROM ELEMENT
C=======================================================================

           P = PME1
           NLEFT = N - NEL
           DO 260 PME = PME1, PME2
              I = IW (PME)
              NVI = -NV (I)
              IF (NVI .GT. 0) THEN
C                I IS A PRINCIPAL VARIABLE IN LME
C                RESTORE NV (I) TO SIGNIFY THAT I IS PRINCIPAL
                 NV (I) = NVI

C                -------------------------------------------------------
C                COMPUTE THE EXTERNAL DEGREE (ADD SIZE OF CURRENT ELEM)
C                -------------------------------------------------------

                 DEG = MAX (1, MIN (DEGREE (I) + DEGME-NVI, NLEFT-NVI))

C                -------------------------------------------------------
C                PLACE THE SUPERVARIABLE AT THE HEAD OF THE DEGREE LIST
C                -------------------------------------------------------

                 INEXT = HEAD (DEG)
                 IF (INEXT .NE. 0) LAST (INEXT) = I
                 NEXT (I) = INEXT
                 LAST (I) = 0
                 HEAD (DEG) = I

C                -------------------------------------------------------
C                SAVE THE NEW DEGREE, AND FIND THE MINIMUM DEGREE
C                -------------------------------------------------------

                 MINDEG = MIN (MINDEG, DEG)
                 DEGREE (I) = DEG

C                -------------------------------------------------------
C                PLACE THE SUPERVARIABLE IN THE ELEMENT PATTERN
C                -------------------------------------------------------

                 IW (P) = I
                 P = P + 1
                 ENDIF
260           CONTINUE

C=======================================================================
C  FINALIZE THE NEW ELEMENT
C=======================================================================

           NV (ME) = NVPIV + DEGME
C          NV (ME) IS NOW THE DEGREE OF PIVOT (INCLUDING DIAGONAL PART)
C          SAVE THE LENGTH OF THE LIST FOR THE NEW ELEMENT ME
           LEN (ME) = P - PME1
           IF (LEN (ME) .EQ. 0) THEN
C             THERE IS NOTHING LEFT OF THE CURRENT PIVOT ELEMENT
              PE (ME) = 0
              W (ME) = 0
              ENDIF
           IF (NEWMEM .NE. 0) THEN
C             ELEMENT WAS NOT CONSTRUCTED IN PLACE: DEALLOCATE PART
C             OF IT (FINAL SIZE IS LESS THAN OR EQUAL TO NEWMEM,
C             SINCE NEWLY NONPRINCIPAL VARIABLES HAVE BEEN REMOVED).
              PFREE = P
              MEM = MEM - NEWMEM + LEN (ME)
              ENDIF

C=======================================================================
C          END WHILE (SELECTING PIVOTS)
           GOTO 30
           ENDIF
C=======================================================================

C=======================================================================
C  COMPUTE THE PERMUTATION VECTORS
C=======================================================================

C       ----------------------------------------------------------------
C       THE TIME TAKEN BY THE FOLLOWING CODE IS O(N).  AT THIS
C       POINT, ELEN (E) = -K HAS BEEN DONE FOR ALL ELEMENTS E,
C       AND ELEN (I) = 0 HAS BEEN DONE FOR ALL NONPRINCIPAL
C       VARIABLES I.  AT THIS POINT, THERE ARE NO PRINCIPAL
C       SUPERVARIABLES LEFT, AND ALL ELEMENTS ARE ABSORBED.
C       ----------------------------------------------------------------

C       ----------------------------------------------------------------
C       COMPUTE THE ORDERING OF UNORDERED NONPRINCIPAL VARIABLES
C       ----------------------------------------------------------------


        DO 290 I = 1, N
           IF (ELEN (I) .EQ. 0) THEN

C             ----------------------------------------------------------
C             I IS AN UN-ORDERED ROW.  TRAVERSE THE TREE FROM I UNTIL
C             REACHING AN ELEMENT, E.  THE ELEMENT, E, WAS THE
C             PRINCIPAL SUPERVARIABLE OF I AND ALL NODES IN THE PATH
C             FROM I TO WHEN E WAS SELECTED AS PIVOT.
C             ----------------------------------------------------------

              J = -PE (I)
C             WHILE (J IS A VARIABLE) DO:
270           CONTINUE
              IF (ELEN (J) .GE. 0) THEN
                 J = -PE (J)
                 GOTO 270
                 ENDIF
              E = J

C             ----------------------------------------------------------
C             GET THE CURRENT PIVOT ORDERING OF E
C             ----------------------------------------------------------

              K = -ELEN (E)

C             ----------------------------------------------------------
C             TRAVERSE THE PATH AGAIN FROM I TO E, AND COMPRESS THE
C             PATH (ALL NODES POINT TO E).  PATH COMPRESSION ALLOWS
C             THIS CODE TO COMPUTE IN O(N) TIME.  ORDER THE UNORDERED
C             NODES IN THE PATH, AND PLACE THE ELEMENT E AT THE END.
C             ----------------------------------------------------------

              J = I
C             WHILE (J IS A VARIABLE) DO:
280           CONTINUE
              IF (ELEN (J) .GE. 0) THEN
                 JNEXT = -PE (J)
                 PE (J) = -E
                 IF (ELEN (J) .EQ. 0) THEN
C                   J IS AN UNORDERED ROW
                    ELEN (J) = K
                    K = K + 1
                    ENDIF
                 J = JNEXT
                 GOTO 280
                 ENDIF
C             LEAVE ELEN (E) NEGATIVE, SO WE KNOW IT IS AN ELEMENT
              ELEN (E) = -K
              ENDIF
290        CONTINUE

C       ----------------------------------------------------------------
C       RESET THE INVERSE PERMUTATION (ELEN (1..N)) TO BE POSITIVE,
C       AND COMPUTE THE PERMUTATION (LAST (1..N)).
C       ----------------------------------------------------------------

        DO 300 I = 1, N
           K = ABS (ELEN (I))
           LAST (K) = I
           ELEN (I) = K
300        CONTINUE

C=======================================================================
C  RETURN THE MEMORY USAGE IN IW
C=======================================================================

C       IF MAXMEM IS LESS THAN OR EQUAL TO IWLEN, THEN NO COMPRESSIONS
C       OCCURRED, AND IW (MAXMEM+1 ... IWLEN) WAS UNUSED.  OTHERWISE
C       COMPRESSIONS DID OCCUR, AND IWLEN WOULD HAVE HAD TO HAVE BEEN
C       GREATER THAN OR EQUAL TO MAXMEM FOR NO COMPRESSIONS TO OCCUR.
C       RETURN THE VALUE OF MAXMEM IN THE PFREE ARGUMENT.

        PFREE = MAXMEM
        END
