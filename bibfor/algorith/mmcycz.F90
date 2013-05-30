subroutine mmcycz(defico, resoco)
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
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE)
!
! DETECTION DES CYCLAGES - REMISE A ZERO DES DETECTEURS
!
! ----------------------------------------------------------------------
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
!
!
!
!
    integer :: ntpc, iptc, icyc
    character(len=24) :: cyclis, cycnbr, cyctyp, cycpoi, cycgli
    integer :: jcylis, jcynbr, jcytyp, jcypoi, jcygli
    logical :: lctcd, lxfcm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FORMULATION
!
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
    if (lctcd .or. lxfcm) goto 99
!
! --- ACCES OBJETS
!
    cyclis = resoco(1:14)//'.CYCLIS'
    cycnbr = resoco(1:14)//'.CYCNBR'
    cyctyp = resoco(1:14)//'.CYCTYP'
    cycpoi = resoco(1:14)//'.CYCPOI'
    cycgli = resoco(1:14)//'.CYCGLI'
    call jeveuo(cyclis, 'E', jcylis)
    call jeveuo(cycnbr, 'E', jcynbr)
    call jeveuo(cyctyp, 'E', jcytyp)
    call jeveuo(cycpoi, 'E', jcypoi)
    call jeveuo(cycgli, 'E', jcygli)
!
! --- INITIALISATIONS
!
    ntpc = cfdisi(defico,'NTPC' )
!
! --- RAZ
!
    do 50 iptc = 1, ntpc
        do 55 icyc = 1, 4
            zi(jcylis-1+4*(iptc-1)+icyc) = 0
            zi(jcynbr-1+4*(iptc-1)+icyc) = 0
            zi(jcytyp-1+4*(iptc-1)+icyc) = 0
            zk16(jcypoi-1+4*(iptc-1)+icyc) = ' '
55      continue
        zr(jcygli-1+3*(iptc-1)+1) = 0.d0
        zr(jcygli-1+3*(iptc-1)+2) = 0.d0
        zr(jcygli-1+3*(iptc-1)+3) = 0.d0
50  end do
!
99  continue
!
    call jedema()
end subroutine
