function xvfimo(modele, fiss)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=8) :: modele, fiss
    logical :: xvfimo
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!     ROUTINE UTILITAIRE POUR X-FEM
!
!     BUT : RENVOIE .TRUE. SI LA FISSURE EST ASSOCIEE AU MODELE
!
!  IN :
!     MODELE : NOM DE LA SD_MODELE
!     FISS   : NOM DE LA SD_FISS_XFEM
! ======================================================================
!
    integer :: ier, nfiss, jfiss, i
    character(len=8) :: k8b
!
    call jemarq()
!
    call jeexin(modele//'.FISS', ier)
    call assert(ier.ne.0)
!
!     RECUPERATION DU NOMBRE DE FISSURES ASSOCIEES AU MODELE
    call dismoi('F', 'NB_FISS_XFEM', modele, 'MODELE', nfiss,&
                k8b, ier)
!
!     RECUPERATION DE LA LISTE DES FISSURES ASSOCIEES AU MODELE
    call jeveuo(modele//'.FISS', 'L', jfiss)
!
    xvfimo=.false.
!
    do 10 i = 1, nfiss
        if (fiss .eq. zk8(jfiss-1+i)) xvfimo=.true.
10  end do
!
    call jedema()
!
end function
