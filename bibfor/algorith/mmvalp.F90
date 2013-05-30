subroutine mmvalp(ndim, alias, nno, ncmp, ksi1,&
                  ksi2, valend, valept)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mmnonf.h'
    integer :: ndim, nno, ncmp
    character(len=8) :: alias
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: valend(*), valept(*)
!
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL D'UNE COMPOSANTE D'UN CHAMP EN UN POINT DONNE D'UNE MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
! IN  NNO    : NOMBRE DE NOEUD DE L'ELEMENT
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  NCMP   : NOMBRE DE COMPOSANTE
! IN  KSI1   : COORDONNEE KSI1 SUR LA MAILLE
! IN  KSI2   : COORDONNEE KSI2 SUR LA MAILLE
! IN  VALEND : VALEUR DU CHAMP AUX NOEUDS
! OUT VALEPT : VALEUR DU CHAMP SUR LE POINT
!
!
!
!
    real(kind=8) :: ff(9)
    integer :: ino, icmp
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do 1 icmp = 1, ncmp
        valept(icmp) = 0.d0
 1  end do
    call assert(nno.le.9)
!
! --- FONCTIONS DE FORME
!
    call mmnonf(ndim, nno, alias, ksi1, ksi2,&
                ff)
!
! --- CALCUL
!
    do 40 icmp = 1, ncmp
        do 10 ino = 1, nno
            valept(icmp) = ff(ino)*valend((ino-1)*ncmp+icmp) + valept(icmp)
10      continue
40  end do
!
    call jedema()
end subroutine
