subroutine lxscan(chin, ideb, iclass, ival, rval,&
                  cval)
    implicit none
#include "asterc/ismaem.h"
    character(len=*) :: chin, cval
    integer :: ideb, iclass, ival
    real(kind=8) :: rval
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!                          ANALYSEUR LEXICAL
!     ------------------------------------------------------------------
! IN  CHIN   CHAINE A DECODER
! VAR IDEB   INDICE DE DEBUT DE LA CHAINE A DECODER            (IN)
!            INDICE DE DEBUT DE LA CHAINE SUIVANTE A DECODER   (OUT)
! OUT ICLASS CLASSE DE L'ITEM TROUVE
!           -- TYPE -----    ---- INFORMATION --------------------------
!      -1   FIN D'ENREGISTREMENT (RIEN A LIRE)
!       0   ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!       1   ENTIER           IVAL DE TYPE INTEGER
!       2   REEL             RVAL DE TYPE REAL*8
!       3   IDENTIFICATEUR   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!       4   TEXTE            CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!       5   SEPARATEUR       CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
!     ------------------------------------------------------------------
!     ROUTINE(S) FORTRAN     :
!         ICHAR
!     ------------------------------------------------------------------
! FIN LXSCAN
!     ------------------------------------------------------------------
!
!     --- VARIABLES GLOBALES -------------------------------------------
!-----------------------------------------------------------------------
    integer :: ietat, iexp, ilire, isigne, kclass, kdeb
    integer :: lgmax, maxexp, minexp, mxchar, mxcla1, mxclas, mxdeli
    integer :: mxetat
    real(kind=8) :: rinfin
!-----------------------------------------------------------------------
    parameter ( mxclas = 10 , mxchar = 255 , mxdeli = 15 )
!
    integer :: clnum, cllet, clsig, clpnt, clexp, clquo, clbls, clbl, clill
    integer :: cleor, nbdeli
    common /lxcn01/   clnum , cllet , clsig , clpnt , clexp , clquo ,&
     &                  clbls , clbl  , clill , cleor , nbdeli
!
    character(len=1) :: class(0:mxchar), cldeli(mxdeli)
    common /lxcc01/   class           , cldeli
!
    common /lxfi00/ isigne
!     --- VARIABLES LOCALES --------------------------------------------
    parameter ( mxetat = 13 , mxcla1 = mxclas+1)
    integer :: neweta(mxcla1, mxetat)
!
    logical(kind=1) :: nbeneg, expneg
    character(len=1) :: carext
    real(kind=8) :: xndec, xdec
    integer :: nival, inival
    character(len=80) :: serr
!
!     ------------------------------------------------------------------
!                       TABLE DES ETATS ET DES ACTIONS
!     ------------------------------------------------------------------
!  CLASSE  01  02  03  04  05  06  07  08  09  10  11
!          NBE LET  +-  .  ED   '   _      AR  EOR DELIM
!  AR EST MIS ICI POUR AROBASE (CARACTERE INTERDIT)
!  DEBUT
!  NUMERIQUES
!  CHAINE QUOTEE
!  IDENTIFIEUR
! CORRECTION BOGUE '.E+'
    data       neweta/&
     &     03 ,12, 02, 13, 12, 09, 00, 01, 00, -6,-15,&
     &     03 ,-7, -7, 04, -7, 00, 00, -7, 00, -7, -7,&
     &     03 ,00, -1, 04, 00, 00, 00, -1, 00, -1, -1,&
     &     05 ,00, -2, 00, 06, 00, 00, -2, 00, -2, -2,&
     &     05 ,00, -2, 00, 06, 00, 00, -2, 00, -2, -2,&
     &     08 ,00, 07, 00, 00, 00, 00, 07, 00,  0,  0,&
     &     08 ,00, 00, 00, 00, 00, 00, 00, 00,  0,  0,&
     &     08 ,00, -2, 00, 00, 00, 00, -2, 00, -2, -2,&
     &     10 ,10, 10, 10, 10, 11, 10, 10, 10, 00, 10,&
     &     10 ,10, 10, 10, 10, 11, 10, 10, 10, 00, 10,&
     &     00 ,00, 00, 00, 00, 10, 00, -4, 00, -4, -4,&
     &     12 ,12, -3, -3, 12, 00, 12, -3, 00, -3, -3,&
     &     05 ,-5, 00, 00, -5, 00, 00, 00, 00, 00, 00/
!     ------------------------------------------------------------------
!     SORTIES DE LA TABLE : VOIR ICLASS ,
!     LES SORTIES SONT NEGATIVES ET LA DIZAINE INDIQUE QU'IL FAUT LIRE
!     UN CARACTERE D'AVANCE
!     ------------------------------------------------------------------
    data   minexp / -78     /
    data   maxexp /  75     /
    data   rinfin / 1.d75   /
!     ------------------------------------------------------------------
!     FONCTIONS INTRINSEQUES
#define classe(carext) ichar(class(ichar(carext)))
#define num(carext) ichar(carext)-ichar('0')
#define lclass(kclass) min(kclass,mxcla1)
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    ietat = 1
    lgmax = len(chin)
    ival = 0
    kdeb = ideb - 1
    nbeneg = .false.
    isigne = 0
    expneg = .true.
!
 10 continue
    kdeb = kdeb + 1
    if (kdeb .le. lgmax) then
        carext = chin(kdeb:kdeb)
        kclass = lclass(classe(carext))
    else
        kclass = cleor
    endif
    ietat = neweta(kclass,ietat)
!
!   ------------------ EXECUTION DE L'ACTION ASSOCIEE -------------
    select case (ietat)
    case (1)
!       --- ELIMINATION DES BLANCS ---
        ideb = ideb+1
    case (2)
!       --- NUMERIQUE : SIGNE ---
        nbeneg = carext.eq.'-'
        isigne = 1
    case (3)
!       --- NUMERIQUE : PARTIE ENTIERE ---
!       TEST VALEURE ENTIERE < MAX_INT
        nival=ival*10
        inival=nival/10
        if (inival .ne. ival) then
!           ON AVANCE JUSQU'A TROUVER UN BLANC, UN DELIMITEUR OU EOR
101         continue
            kdeb = kdeb+1
            if (kdeb .le. lgmax) then
                carext = chin(kdeb:kdeb)
                kclass = lclass(classe(carext))
                if (kclass .le. mxclas .and. kclass .ne. clbl) goto 101
            endif
            ival = kdeb-ideb
            cval = chin(ideb:kdeb-1)
            write (serr,*) 'VALEUR ENTIERE TROP GRANDE   ( MAX = ', ismaem(),')'
            goto 102
        endif
        ival = ival*10 + num(carext)
102     continue
    case (4)
!           --- NUMERIQUE : POINT DECIMAL ---
        iexp = 0
        xndec = 1.0d0
        xdec = 0.d0
    case (5)
!           --- NUMERIQUE : PARTIE DECIMALE ---
        xndec = xndec * 10
        xdec = xdec + dble(num(carext)) / xndec
    case (6)
!           --- NUMERIQUE : EXPOSANT RENCONTRE  ---
        expneg = .false.
    case (7)
!           --- NUMERIQUE : SIGNE DE L'EXPOSANT ---
        expneg = carext.eq.'-'
    case (8)
!           --- NUMERIQUE : MODIFICATION DE L'EXPOSANT ---
        iexp = iexp*10 + num(carext)
    case (9)
!           --- NUMERIQUE : POINT DECIMAL ---
        iexp = 0
        xndec = 1.0d0
        xdec = 0.d0
    case (10)
!           --- CHAINE : SAISIE DU TEXTE ---
        ival = ival + 1
        cval(ival:ival) = carext
    case (11)
!           --- CHAINE : QUOTE FINAL OU DOUBLE QUOTE ---
        continue
    case (12)
!           --- IDENT : SAISIE DE L'IDENTIFICATEUR ---
        ival = ival + 1
        cval(ival:ival) = carext
    case (13)
!           --- NUMERIQUE : POINT DECIMAL EN PREMIERE POSITION ---
        iexp = 0
        xndec = 1.0d0
        xdec = 0.d0
    case default
        goto 20
    end select
    goto 10
!
 20 continue
    ilire = - ietat / 10
    iclass = mod(-ietat,10)
    select case (iclass + 1)
case (1)
!    --- UNE ERREUR A ETE DETECTEE ---
!       ON AVANCE JUSQU'A TROUVER UN BLANC, UN DELIMITEUR OU EOR
201 continue
    kdeb = kdeb+1
    if (kdeb .le. lgmax) then
        carext = chin(kdeb:kdeb)
        kclass = lclass(classe(carext))
        if (kclass .le. mxclas .and. kclass .ne. clbl) then
            goto 201
        endif
    endif
    ival = kdeb-ideb
    cval = chin(ideb:kdeb-1)
case (2)
!    --- UN ENTIER A ETE RECONNU ---
    if (nbeneg) then
        ival = -ival
    endif
case (3)
!    --- UN REEL A ETE RECONNU ---
    if (expneg) then
        iexp = -iexp
    endif
    rval = dble(ival) + xdec
    if (iexp .lt. minexp) then
        rval = 0
        write(serr,*)'EXPOSANT TROP PETIT ( LIMITE = ',minexp,')'
    else if (iexp .gt. maxexp) then
        rval = rinfin
        write(serr,*)'EXPOSANT TROP GRAND ( LIMITE = ',maxexp,')'
    else
        rval = rval * (10.d0**iexp)
    endif
    iclass = 2
    if (nbeneg) then
        rval = -rval
    endif
!       REMISE A ZERO DE IVAL POUR EVITER CERTAINES REMANENCES
    ival=0
case (4)
!    --- UN IDENTIFICATEUR A ETE RECONNU ---
    continue
case (5)
!    --- UN TEXTE A ETE RECONNU ---
    continue
case (6)
!    --- UN SEPARATEUR A ETE RECONNU ---
    ival = 1
    cval = carext
    if (ilire .eq. 1) then
        kdeb = kdeb+1
    endif
case (7)
!    --- UNE FIN DE LIGNE A ETE RECONNUE ---
    iclass = -1
case (8)
!    --- UN + OU UN - ISOLE A ETE TROUVE ---
    if (nbeneg) then
        carext = '-'
    else
        carext = '+'
    endif
    ival = 1
    cval = carext
!       --- EN FAIT IL FAUT TRAITER AU NIVEAU SUPERIEUR ---
    iclass = 5
end select
!
    ideb = kdeb
!
end subroutine
