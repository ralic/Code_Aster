subroutine acevco(nbocc, nlm, nlg, ier)
    implicit none
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/u2mess.h'
    integer :: nbocc, nlm, nlg, ier
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!                          AFFE_CARA_ELEM
!
!        VERIFICATION DES MOTS CLES POUR L'ELEMENT COQUE
!
! ----------------------------------------------------------------------
!  IN
!     NBOCC  : NOMBRE D'OCCURENCE
!  OUT
!     NLM    : NOMBRE TOTAL DE MAILLE
!     NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
!  IN/OUT
!     IER    : CUMUL DES ERREURS
! ----------------------------------------------------------------------
    integer :: iarg, ioc, nco, ne, nef, nex, nexf, ng, nin
    integer :: nk, nm, nsom
    real(kind=8) :: r8b
    character(len=8) :: k8b, nomu
    character(len=16) :: concep, cmd
!-----------------------------------------------------------------------
    call getres(nomu, concep, cmd)
!
    nlm = 0
    nlg = 0
    do 10 ioc = 1, nbocc
        call getvtx('COQUE', 'GROUP_MA', ioc, iarg, 0,&
                    k8b, ng)
        call getvtx('COQUE', 'MAILLE', ioc, iarg, 0,&
                    k8b, nm)
        call getvr8('COQUE', 'EPAIS', ioc, iarg, 0,&
                    r8b, ne)
        call getvid('COQUE', 'EPAIS_FO', ioc, iarg, 0,&
                    k8b, nef)
        call getvr8('COQUE', 'A_CIS', ioc, iarg, 0,&
                    r8b, nk)
        call getvr8('COQUE', 'EXCENTREMENT', ioc, iarg, 0,&
                    r8b, nex)
        call getvid('COQUE', 'EXCENTREMENT_FO', ioc, iarg, 0,&
                    k8b, nexf)
        call getvtx('COQUE', 'INER_ROTA', ioc, iarg, 0,&
                    k8b, nin)
        call getvtx('COQUE', 'MODI_METRIQUE', ioc, iarg, 0,&
                    k8b, nco)
!
        if (ioc .eq. 1 .and. abs(ne+nef) .ne. 1) then
            call u2mess('E', 'MODELISA_53')
            ier = ier + 1
        endif
!
        if ((nex+nexf) .ne. 0 .and. nin .ne. 0) then
            call getvtx('COQUE', 'INER_ROTA', ioc, iarg, 1,&
                        k8b, nin)
            if (k8b .eq. 'NON') then
                call u2mess('E', 'MODELISA_54')
                ier = ier + 1
            endif
        endif
!
        nsom = ng + nm
        if (nsom .eq. ng .or. nsom .eq. nm) then
            nlm = max(nlm,-nm)
            nlg = max(nlg,-ng)
        endif
10  end do
!
end subroutine
