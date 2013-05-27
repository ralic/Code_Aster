subroutine ndaram(result, sddyna, numarc)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/codent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndynin.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/wkvect.h'
    include 'blas/dcopy.h'
    character(len=8) :: result
    integer :: numarc
    character(len=19) :: sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! ARCHIVAGE DEPL/VITE/ACCE GENERALISES DANS SD_RESULT
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
! IN  NUMARC : NUMERO DE L'ARCHIVAGE
! IN  SDDYNA : SD DYNAMIQUE
!
!
!
!
    integer :: jpara
    character(len=8) :: k8bid
    character(len=6) :: chford
    character(len=19) :: dgen, vgen, agen
    integer :: jrestd, jrestv, jresta
    integer :: nbmodp
    character(len=24) :: trgene
    character(len=19) :: depgep, vitgep, accgep
    integer :: jdepgp, jvitgp, jaccgp
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM TRAN_GENE_NOLI
!
    call codent(numarc, 'D0', chford)
    trgene = result(1:8)//'.TRG'//chford(1:6)
!
! --- ARCHIVAGE NOM TRAN_GENE_NOLI
!
    call rsadpa(result, 'E', 1, 'TRAN_GENE_NOLI', numarc,&
                0, jpara, k8bid)
    zk24(jpara) = trgene
    nbmodp = ndynin(sddyna,'NBRE_MODE_PROJ')
!
! --- NOM VECTEURS DEPL/VITE/ACCE GENERALISES
!
    dgen = trgene(1:18)//'D'
    vgen = trgene(1:18)//'V'
    agen = trgene(1:18)//'A'
!
! --- CREATION DEPL/VITE/ACCE GENERALISES
!
    call wkvect(dgen, 'G V R', nbmodp, jrestd)
    call wkvect(vgen, 'G V R', nbmodp, jrestv)
    call wkvect(agen, 'G V R', nbmodp, jresta)
!
! --- ARCHIVAGE
!
    call ndynkk(sddyna, 'PRMO_DEPGEP', depgep)
    call ndynkk(sddyna, 'PRMO_VITGEP', vitgep)
    call ndynkk(sddyna, 'PRMO_ACCGEP', accgep)
    nbmodp = ndynin(sddyna,'NBRE_MODE_PROJ')
    call jeveuo(accgep, 'L', jaccgp)
    call jeveuo(vitgep, 'L', jvitgp)
    call jeveuo(depgep, 'L', jdepgp)
    call dcopy(nbmodp, zr(jdepgp), 1, zr(jrestd), 1)
    call dcopy(nbmodp, zr(jvitgp), 1, zr(jrestv), 1)
    call dcopy(nbmodp, zr(jaccgp), 1, zr(jresta), 1)
!
    call jedema()
!
end subroutine
