subroutine mvnume(depmoi, depdel, depplu)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/copisd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/vtcmbl.h'
    include 'asterfort/vtcopy.h'
    include 'asterfort/vtzero.h'
    character(len=19) :: depmoi, depdel, depplu
!
! ----------------------------------------------------------------------
!
! ROUTINE CALCUL (INITIALISATION)
!
! PREPARATION DEPLACEMENTS AVEC TRANSFERT DE NUMEROTATION
!
! ----------------------------------------------------------------------
!
! IN  DEPMOI : DEPLACEMENTS EN T-
! IN  DEPDEL : INCREMENT DE DEPLACEMENT
! OUT DEPPLU : DEPLACEMENTS EN T+
!
! ----------------------------------------------------------------------
!
    integer :: ibid, iret
    character(len=19) :: pfchn1, pfchn2
    character(len=1) :: typcst(2), typech(2), typres
    real(kind=8) :: const(2)
    character(len=24) :: nomch(2), chpres, depmo1
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    depmo1 = '&&MVNUME.DEPMO1'
!
! --- SI LES CHAMPS N'ONT PAS LA MEME NUMEROTATION, ON TRANSFERT DEPMOI
! --- DANS LA NUMEROTATION DE DEPDEL
!
    call dismoi('F', 'PROF_CHNO', depmoi, 'CHAM_NO', ibid,&
                pfchn1, ibid)
    call dismoi('F', 'PROF_CHNO', depdel, 'CHAM_NO', ibid,&
                pfchn2, ibid)
    if (pfchn1 .ne. pfchn2) then
        call copisd('CHAMP_GD', 'V', depdel, depmo1)
        call vtzero(depmo1)
        call vtcopy(depmoi, depmo1, 'F', iret)
    else
        depmo1 = depmoi
    endif
!
! --- ON CALCULE LE CHAMP DEPPLU=DEPMO1+DEPDEL
!
    typcst(1) = 'R'
    typcst(2) = 'R'
    const(1) = 1.d0
    const(2) = 1.d0
    typech(1) = 'R'
    typech(2) = 'R'
    nomch(1) = depmo1
    nomch(2) = depdel
    typres = 'R'
    chpres = depplu
    call vtcmbl(2, typcst, const, typech, nomch,&
                typres, chpres)
!
    call jedema()
end subroutine
