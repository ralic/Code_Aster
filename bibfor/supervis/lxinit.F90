subroutine lxinit()
! aslint: disable=
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     INITIALISATION DE L'ANALYSEUR LEXICAL
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         (CF ARGUMENT)
!     ROUTINE(S) FORTRAN     :
!         CHAR    ICHAR
!     ------------------------------------------------------------------
! FIN LXINIT
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
#include "asterfort/lxdeli.h"
    integer :: i, mxchar, mxclas, mxcols, mxdeli, nbdeli
!-----------------------------------------------------------------------
    parameter ( mxclas = 10 , mxchar = 255 , mxdeli = 15 )
    integer :: clnum, cllet, clsig, clpnt, clexp, clquo, clbls, clbl, clill
    integer :: cleor
!
    common /lxcn01/   clnum , cllet , clsig , clpnt , clexp , clquo ,&
     &                  clbls , clbl  , clill , cleor , nbdeli
!
    character(len=1) :: class(0:mxchar), cldeli(mxdeli)
    common /lxcc01/    class          , cldeli
!
!     ------------------------------------------------------------------
    parameter  ( mxcols = 80 )
    character(len=mxcols) :: chaine
    character(len=1) :: kclass
!     ------------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
!                     DEFINITION DES CLASSES SIMPLES
!     CLNUM  =  1 : NUMERIQUES        CLLET  =  2 : LETTRES
!     CLSIG  =  3 : SIGNE + -         CLPNT  =  4 : POINT .
!     CLEXP  =  5 : EXPOSANT E D      CLQUO  =  6 : QUOTE '
!     CLBLS  =  7 : BLANC SOULIGNE _  CLBL   =  8 : BLANC
!     CLILL  =  9 : ILLEGAUX          CLEOR  = 10 : FIN D'ENREGISTREMENT
!     ------------------------------------------------------------------
!
    clnum = 1
    cllet = 2
    clsig = 3
    clpnt = 4
    clexp = 5
    clquo = 6
    clbls = 7
    clbl = 8
    clill = 9
    cleor = 10
!     ------------------------------------------------------------------
!
!
!     INITIALISATION DES CLASSES A ILLEGAL /* OPTION PAR DEFAUT */
    kclass = char(clill)
    do 10 i = 0, mxchar
        class(i) = kclass
10  end do
!
!     INITIALISATION DE LA CLASSE NUMERIQUE
    chaine = '0123456789'
    kclass = char(clnum)
    do 20 i = 1, 10
        class(ichar(chaine(i:i))) = kclass
20  end do
!
!     INITIALISATION DE LA CLASSE ALPHABETIQUE
    chaine = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'// 'abcdefghijklmnopqrstuvwxyz'
    kclass = char(cllet)
    do 30 i = 1, 52
        class(ichar(chaine(i:i))) = kclass
30  end do
!
!     INITIALISATION DE LA CLASSE SIGNE
    class(ichar('+')) = char(clsig)
    class(ichar('-')) = char(clsig)
!
!     INITIALISATION DE LA CLASSE EXPOSANT
    class(ichar('E')) = char(clexp)
    class(ichar('e')) = char(clexp)
    class(ichar('D')) = char(clexp)
    class(ichar('d')) = char(clexp)
!
!     INITIALISATION DE LA CLASSE QUOTE BLANC BLANC_SOULIGNE ET POINT
    class(ichar('''')) = char(clquo)
    class(ichar(' ')) = char(clbl )
    class(ichar('_')) = char(clbls)
    class(ichar('.')) = char(clpnt)
!
!     TABULATION
    class(9) = char(clbl )
!
!     INITIALISATION DE LA CLASSE 'DELIMITEUR'
    nbdeli = mxdeli
    call lxdeli(cldeli, nbdeli)
    kclass = char(clill)
    do 40 i = 1, nbdeli
        if (class(ichar(cldeli(i))) .eq. kclass) then
            class(ichar(cldeli(i))) = char(mxclas+i)
        endif
40  end do
!
!     ------------------------------------------------------------------
!
end subroutine
