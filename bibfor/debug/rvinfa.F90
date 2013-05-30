subroutine rvinfa(ifm, mcf, iocc, qnt, opt,&
                  opr, rep)
    implicit   none
    include 'asterc/getvr8.h'
    integer :: ifm
    character(len=*) :: mcf, qnt, opt, opr, rep
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     AFFICHAGE DES INFO SUR LA QUANTITE TRAITE
!     ------------------------------------------------------------------
! IN  REP    : K : REPERE
! IN  OPT    : K : OPTION
! IN  QNT    : K : QUANTITE
! IN  OPR    : K : OPERATION
!     ------------------------------------------------------------------
!
    character(len=80) :: mess
    integer :: iocc, pt, n1
    real(kind=8) :: poin(3)
    integer :: iarg
!
    mess = ' '
    pt = 1
    if (opr(1:1) .eq. 'E') then
        mess(pt:pt+10) = 'EXTRACTION'
        pt = pt + 12
    else if (opr(1:1) .eq. 'S') then
        mess(pt:pt+17) = 'RESULTANTE_MOMENT'
        pt = pt + 19
    else if (opr(1:7) .eq. 'MOYENNE') then
        mess(pt:pt+7) = 'MOYENNE'
        pt = pt + 9
    else if (opr(1:4) .eq. 'RCCM') then
        mess(pt:pt+11) = 'RCC-M B3200'
        pt = pt + 13
    else
    endif
    if (qnt(1:7) .eq. 'TRACE_N') then
        mess(pt:pt+13) = 'TRACE_NORMALE'
        pt = pt + 15
    else if (qnt(1:7) .eq. 'TRACE_D') then
        mess(pt:pt+19) = 'TRACE_DIRECTIONELLE'
        pt = pt + 21
    else if (qnt(1:7) .eq. 'INVARIA') then
        mess(pt:pt+10) = 'INVARIANTS'
        pt = pt + 12
    else if (qnt(1:7) .eq. 'ELEM_PR') then
        mess(pt:pt+19) = 'ELEMENTS_PRINCIPAUX'
        pt = pt + 21
    else
    endif
!
    if ((opt(1:4) .eq. 'SIGM') .or. (opt(1:4) .eq. 'SIEF')) then
        mess(pt:80) = 'TENSEUR CONTRAINTE'
    else if (opt(1:4) .eq. 'EPSI') then
        mess(pt:80) = 'TENSEUR DEFORMATION'
    else if (opt(1:4) .eq. 'EFGE') then
        mess(pt:80) = 'TENSEURS MOMENT_FLECHISSANT EFFORT_GENERALISE'
    else if (opt(1:4) .eq. 'DEPL') then
        mess(pt:80) = 'DEPLACEMENTS'
    else if (opt(1:4) .eq. 'TEMP') then
        mess(pt:80) = 'TEMPERATURE'
    else if (opt(1:4) .eq. 'FORC') then
        mess(pt:80) = 'FORCE'
    else if (opt(1:4) .eq. 'FLUX') then
        mess(pt:80) = 'FLUX'
    else
    endif
!
    write(ifm,*)' '
    write(ifm,*)mess
!
    if (opr(1:1) .eq. 'S' .and. mcf(1:6) .eq. 'ACTION') then
        call getvr8(mcf, 'POINT', iocc, iarg, 0,&
                    poin, n1)
        if (n1 .eq. -2) then
            call getvr8(mcf, 'POINT', iocc, iarg, 2,&
                        poin, n1)
            write(ifm,1000) poin(1) , poin(2)
        else if (n1 .eq. -3) then
            call getvr8(mcf, 'POINT', iocc, iarg, 3,&
                        poin, n1)
            write(ifm,1010) poin(1) , poin(2) , poin(3)
        endif
    endif
!
    if (rep(1:1) .eq. 'G') then
        write(ifm,*)'REPERE GLOBAL'
    else if (rep(1:1) .eq. 'L') then
        write(ifm,*)'REPERE LOCAL'
    else if (rep(1:1) .eq. 'P') then
        write(ifm,*)'REPERE POLAIRE'
    else
    endif
    write(ifm,*)' '
!
    1000 format ( ' MOMENT PAR RAPPORT AU POINT : ',1p,e12.5,1x,e12.5)
    1010 format ( ' MOMENT PAR RAPPORT AU POINT : ',1p,e12.5,1x,e12.5,&
     &                                           1x,e12.5)
!
end subroutine
