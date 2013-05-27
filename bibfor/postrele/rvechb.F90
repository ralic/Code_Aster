subroutine rvechb(epsi, typmai, ndfac, r, valcpm,&
                  nbcp, nbso, nbsi, valcp)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    include 'asterc/r8vide.h'
    character(len=8) :: typmai
    integer :: ndfac(*), nbcp, nbso, nbsi
    real(kind=8) :: epsi, r(*), valcpm(*), valcp(*)
!
!**********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     EVALUATION DE CMP EN UM POINT D' UNE FACE
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     EPSI   : PRECISION
!     TYPMAI : TYPE DE LA MAILLE
!     NDFAC  : NUMERO LOCAL DES NOEUD DE LA FACE (AVEC ORIENTATION)
!     R      : PARAMETRE(S) DE REPERAGE DU POINT SUR LA FACE
!     VALCPM : VALEUR DES CMP SUR LES NOEUD DE LA MAILLE
!              ORGANISATION SUIVANT LA NUMEROTATION LOCALE DES NOEUDS
!     NBCP   : NOMBRE DE CMP
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     VALCP  : TABLE DES VALEURS DES CMPS AU POINT
!
!**********************************************************************
!
!  VARIABLES LOCALES
!  -----------------
    character(len=2) :: dim
    real(kind=8) :: r1
    integer :: nd, nf, ni, i
    integer :: lng
!-----------------------------------------------------------------------
!
!================== CORPS DE LA ROUTINE ================================
!
!
    if ((typmai .eq. 'POI1') .or. (typmai .eq.' SEG2') .or. (typmai .eq.' SEG3')) then
!
        dim = '1D'
!
        else if ( (typmai .eq. 'TRIA3') .or. (typmai .eq. 'TRIA6') .or.&
    (typmai .eq. 'QUAD4') .or. (typmai .eq. 'QUAD8') .or. (typmai&
    .eq. 'QUAD9') ) then
!
        dim = '2D'
!
    else
!
        dim = '3D'
!
    endif
!
    lng = nbsi*nbcp
!
    if (dim .eq. '1D') then
!
        nd = ndfac(1)
!
        do 10, i = 1, nbcp*nbso, 1
!
        valcp(i) = valcpm(i + (nd-1)*lng)
!
10      continue
!
    else if (dim .eq. '2D') then
!
        nd = ndfac(1)
        nf = ndfac(2)
        ni = ndfac(3)
        r1 = r(1)
!
        if (abs(1.0d0 - r1) .le. epsi) then
!
!           /* LE POINT EST LE NOEUD EXTREMITE DE LA FACE */
!
            do 21, i = 1, nbso*nbcp, 1
!
            valcp(i) = valcpm(i + (nf-1)*lng)
!
21          continue
!
        else if (abs(r1) .le. epsi) then
!
!           /* LE POINT EST LE NOEUD ORIGINE DE LA FACE */
!
            do 32, i = 1, nbcp*nbso, 1
!
            valcp(i) = valcpm(i + (nd-1)*lng)
!
32          continue
!
        else
!
!           /* LE POINT N'EST PAS AU BORD DE LA FACE */
!
            if (ni .eq. 0) then
!
!           /* PAS DE NOEUD INTERMEDIAIRE : INTERPOLATION DE DEGRE 1 */
!
                do 41, i = 1, nbcp*nbso, 1
!
                valcp(i) = (1.0d0 - r1)*valcpm(i + (nd-1)*lng) + r1*valcpm(i + (nf-1)*nbcp*nbsi)
!
41              continue
!
            else
!
!           /* NOEUD INTERMEDIAIRE : INTERPOLATION DE DEGRE 2 */
!
                do 52, i = 1, nbcp*nbso, 1
!
                if (valcpm(i+(ni-1)*lng) .eq. r8vide()) then
                    valcp(i) = (1.0d0 - r1)*valcpm(i + (nd-1)*lng) + r1*valcpm(i + (nf-1)*nbcp*nb&
                               &si)
                else
                    valcp(i) = valcpm(&
                               i + (nd-1)*lng) + r1*( ( 4.0d0*valcpm(i + (ni-1)*lng) - 3.0d0*valc&
                               &pm(i + (nd-1)*lng) - valcpm(i + (nf-1)*lng))+ 2.0d0*r1*( valcpm(i&
                               & + (nf-1)*lng)+ valcpm(i + (nd-1)*lng)- 2.0d0* valcpm(i + (ni-1)*&
                               &lng))&
                               )
                endif
!
52              continue
!
            endif
!
        endif
!
    else
!
!        /* TRAITEMENT DU 3D ... */
!
    endif
!
end subroutine
