subroutine nmdocv(mcfact, iocc, algo, nommc, valrmc)
    implicit none
!
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    character(len=14) :: nommc
    character(len=16) :: mcfact, algo
    integer :: iocc
    real(kind=8) :: valrmc
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     RECUPERATION ET VERIFICATION DES MOT-CLES LIES
!     A LA CONVERGENCE LOCALE DES COMPOREMENTS
!     LES MOT-CLES TRAITES SONT RESI_INTE_RELA ET ITER_INTE_MAXI
!
! IN  MCFACT : MOT-CLE FACTEUR (COMP_ELAS OU COMP_INCR)
! IN  IOCC   : NUMERO D'OCCURENCE DU MOT-CLE FACTEUR
! IN  ALGO   : ALGO_INTE (SERT A LA VERIF DE COHERENCE)
! IN  NOMMC  : MOT-CLE A TRAITER ('RESI_INTE_RELA' / 'ITER_INTE_MAXI')
! OUT VALRMC : VALEUR REELLE DU MOT-CLE
!
! ----------------------------------------------------------------------
!
    integer :: iarg, iret, valimc, vali
    real(kind=8) :: rbid
    character(len=16) :: valk(2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     VERIFICATIONS INITIALES
    call assert(mcfact.eq.'COMP_ELAS'.or. mcfact.eq.'COMP_INCR')
!
    call assert(nommc.eq.'RESI_INTE_RELA'.or. nommc.eq.'ITER_INTE_MAXI')
!
!     RECUP DES VALEURS
    if (nommc .eq. 'RESI_INTE_RELA') then
        call getvr8(mcfact, nommc, iocc, iarg, 1,&
                    valrmc, iret)
    else if (nommc.eq.'ITER_INTE_MAXI') then
        call getvis(mcfact, nommc, iocc, iarg, 1,&
                    valimc, iret)
        valrmc = valimc
    endif
!
    call assert(iret.ne.0)
!
!     VERIFICATIONS DE COHERENCE
    if (iarg .eq. 0) then
!        LA VALEUR A ETE FOURNIE PAR L'UTILISATEUR
!        VERIF QUE C'EST BIEN COMPATIBLE AVEC LE RESTE
        if (algo .eq. 'ANALYTIQUE') then
            valk(1)=nommc
            valk(2)=mcfact
            vali=iocc
            call u2mesg('A', 'COMPOR1_70', 2, valk, 1,&
                        vali, 0, rbid)
!          ON MET LA VALEUR A -999 (ON POURRAIT METTRE R8VIDE)
            valrmc = -999.d0
        endif
    endif
!
    if (nommc .eq. 'RESI_INTE_RELA') then
        if (valrmc .gt. 1.0001d-6) call u2mess('A', 'ALGORITH7_60')
    endif
!
    call jedema()
end subroutine
