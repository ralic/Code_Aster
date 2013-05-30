subroutine mmctan(nommai, alias, nno, ndim, coorma,&
                  coorno, itemax, epsmax, tau1, tau2)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mmnewt.h'
    include 'asterfort/u2mesg.h'
    character(len=8) :: nommai, alias
    integer :: itemax, ndim, nno
    real(kind=8) :: epsmax, coorno(3), coorma(27)
    real(kind=8) :: tau1(3), tau2(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! CALCUL DES TANGENTES EN UN NOEUD D'UNE MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMMAI : NOM DE LA MAILLE
! IN  ALIAS  : TYPE DE LA MAILLE
! IN  NNO    : NOMBRE DE NOEUDS DE LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE
! IN  COORMA : CORDONNNES DE LA MAILLE
! IN  COORNO : COORODNNEES DU NOEUD
! IN  ITEMAX : NOMBRE MAXI D'ITERATIONS DE NEWTON POUR LA PROJECTION
! IN  EPSMAX : RESIDU POUR CONVERGENCE DE NEWTON POUR LA PROJECTION
! OUT TAU1   : PREMIERE TANGENTE (NON NORMALISEE)
! OUT TAU2   : SECONDE TANGENTE (NON NORMALISEE)
!
!
!
!
!
    integer :: ifm, niv
    integer :: niverr
    real(kind=8) :: ksi1, ksi2
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- INITIALISATIONS
!
    niverr = 0
!
! --- CALCUL DES VECTEURS TANGENTS DE LA MAILLE EN CE NOEUD
!
    call mmnewt(alias, nno, ndim, coorma, coorno,&
                itemax, epsmax, ksi1, ksi2, tau1,&
                tau2, niverr)
!
! --- GESTION DES ERREURS LORS DU NEWTON LOCAL POUR LA PROJECTION
!
    if (niverr .eq. 1) then
        call u2mesg('F', 'APPARIEMENT_13', 1, nommai, 0,&
                    0, 3, coorno)
    endif
!
    call jedema()
end subroutine
