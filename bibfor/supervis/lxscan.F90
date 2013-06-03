subroutine lxscan(chin, ideb, iclass, ival, rval,&
                  cval)
    implicit none
    include 'asterc/ismaem.h'
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
    logical :: nbeneg, expneg
    character(len=1) :: carext
    integer :: classe
    real(kind=8) :: xndec, xdec
    integer :: nival, inival, lclass, num
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
    classe(carext) = ichar(class(ichar(carext)))
    num   (carext) = ichar(carext)-ichar('0')
    lclass(kclass) = min(kclass,mxcla1)
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
 1  continue
    kdeb = kdeb + 1
    if (kdeb .le. lgmax) then
        carext = chin(kdeb:kdeb)
        kclass = lclass(classe(carext))
    else
        kclass = cleor
    endif
    ietat = neweta(kclass,ietat)
!
!        ------------------ EXECUTION DE L'ACTION ASSOCIEE -------------
    goto (10,20,30,40,50,60,70,80,90,100,110,120,130) ietat
    ilire = - ietat / 10
    iclass = mod(-ietat,10)
!CCC     ICLASS = -IETAT + 10*ILIRE
    goto (1000,1001,1002,1003,1004,1005,1006,1007) iclass+1
!
!           --- ELIMINATION DES BLANCS ---
10  continue
    ideb = ideb+1
    goto 1
!
!           --- NUMERIQUE : SIGNE ---
20  continue
    nbeneg = carext.eq.'-'
    isigne = 1
    goto 1
!
!           --- NUMERIQUE : PARTIE ENTIERE ---
30  continue
!           TEST VALEURE ENTIERE < MAX_INT
    nival=ival*10
    inival=nival/10
    if (inival .ne. ival) then
!              ON AVANCE JUSQU'A TROUVER UN BLANC, UN DELIMITEUR OU EOR
777      continue
        kdeb = kdeb+1
        if (kdeb .le. lgmax) then
            carext = chin(kdeb:kdeb)
            kclass = lclass(classe(carext))
            if (kclass .le. mxclas .and. kclass .ne. clbl) goto 777
        endif
        ival = kdeb-ideb
        cval = chin(ideb:kdeb-1)
        write (serr,*) 'VALEUR ENTIERE TROP GRANDE   ( MAX = ',&
     &              ismaem(),')'
        goto 9999
    endif
    ival = ival*10 + num(carext)
    goto 1
!
!           --- NUMERIQUE : POINT DECIMAL ---
40  continue
    iexp = 0
    xndec = 1.0d0
    xdec = 0.d0
    goto 1
!
!           --- NUMERIQUE : PARTIE DECIMALE ---
50  continue
    xndec = xndec * 10
    xdec = xdec + dble(num(carext)) / xndec
    goto 1
!
!           --- NUMERIQUE : EXPOSANT RENCONTRE  ---
60  continue
    expneg = .false.
    goto 1
!
!           --- NUMERIQUE : SIGNE DE L'EXPOSANT ---
70  continue
    expneg = carext.eq.'-'
    goto 1
!
!           --- NUMERIQUE : MODIFICATION DE L'EXPOSANT ---
80  continue
    iexp = iexp*10 + num(carext)
    goto 1
!
!           --- CHAINE : QUOTE INITIAL ---
90  continue
    goto 1
!
!           --- CHAINE : SAISIE DU TEXTE ---
100  continue
    ival = ival + 1
    cval(ival:ival) = carext
    goto 1
!
!           --- CHAINE : QUOTE FINAL OU DOUBLE QUOTE ---
110  continue
    goto 1
!
!           --- IDENT : SAISIE DE L'IDENTIFICATEUR ---
120  continue
    ival = ival + 1
    cval(ival:ival) = carext
    goto 1
!
!           --- NUMERIQUE : POINT DECIMAL EN PREMIERE POSITION ---
130  continue
    iexp = 0
    xndec = 1.0d0
    xdec = 0.d0
    goto 1
!     ---------------- SORTIE DE L'AUTOMATE ----------------------------
!
!
!        --- UNE ERREUR A ETE DETECTEE ---
1000  continue
!           ON AVANCE JUSQU'A TROUVER UN BLANC, UN DELIMITEUR OU EOR
999  continue
    kdeb = kdeb+1
    if (kdeb .le. lgmax) then
        carext = chin(kdeb:kdeb)
        kclass = lclass(classe(carext))
        if (kclass .le. mxclas .and. kclass .ne. clbl) goto 999
    endif
    ival = kdeb-ideb
    cval = chin(ideb:kdeb-1)
    goto 9999
!
!        --- UN ENTIER A ETE RECONNU ---
1001  continue
    if (nbeneg) ival = -ival
    goto 9999
!
!        --- UN REEL A ETE RECONNU ---
1002  continue
    if (expneg) iexp = -iexp
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
    if (nbeneg) rval = -rval
!           REMISE A ZERO DE IVAL POUR EVITER CERTAINES REMANENCES
    ival=0
    goto 9999
!
!        --- UN IDENTIFICATEUR A ETE RECONNU ---
1003  continue
    goto 9999
!
!        --- UN TEXTE A ETE RECONNU ---
1004  continue
    goto 9999
!
!        --- UN SEPARATEUR A ETE RECONNU ---
1005  continue
    ival = 1
    cval = carext
    if (ilire .eq. 1) kdeb = kdeb+1
    goto 9999
!
!        --- UNE FIN DE LIGNE A ETE RECONNUE ---
1006  continue
    iclass = -1
    goto 9999
!
!        --- UN + OU UN - ISOLE A ETE TROUVE ---
1007  continue
    if (nbeneg) then
        carext = '-'
    else
        carext = '+'
    endif
    ival = 1
    cval = carext
!           --- EN FAIT IL FAUT TRAITER AU NIVEAU SUPERIEUR ---
    iclass = 5
    goto 9999
!
!     ----- SORTIE ---------------------
9999  continue
    ideb = kdeb
end subroutine
