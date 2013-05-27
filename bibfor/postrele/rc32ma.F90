subroutine rc32ma(mater)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/rccome.h'
    include 'asterfort/rcvale.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: mater
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     TRAITEMENT DU CHAM_MATER
!     RECUPERATION POUR CHAQUE ETAT STABILISE
!          DE  E, NU, ALPHA    SOUS ELAS
!          DE  E_REFE          SOUS FATIGUE
!          DE  M_KE, N_KE, SM  SOUS RCCM
!
!     ------------------------------------------------------------------
!
    integer :: nbcmp, nbpa, nbpb, iocc, nbsitu, na, nb, ndim, jvala, jvalb, i
    integer :: nbcmp2
    parameter    ( nbcmp = 7, nbcmp2=nbcmp+1 )
    real(kind=8) :: para(nbcmp), tempa, tempb, tke
    integer :: icodre(nbcmp)
    character(len=8) :: nopa, nopb, typeke, nocmp(nbcmp)
    character(len=16) :: phenom, motclf
    integer :: iarg
! DEB ------------------------------------------------------------------
    call jemarq()
!
!    RECUP TYPE KE
    call getvtx(' ', 'TYPE_KE', 0, iarg, 1,&
                typeke, nb)
    if (typeke .eq. 'KE_MECA') then
        tke=-1.d0
    else
        tke=1.d0
    endif
!
    motclf = 'SITUATION'
    call getfac(motclf, nbsitu)
!
    call rccome(mater, 'ELAS', phenom, icodre)
    if (icodre(1) .eq. 1) call u2mesk('F', 'POSTRCCM_7', 1, 'ELAS')
!
    call rccome(mater, 'FATIGUE', phenom, icodre)
    if (icodre(1) .eq. 1) call u2mesk('F', 'POSTRCCM_7', 1, 'FATIGUE')
!
    call rccome(mater, 'RCCM', phenom, icodre)
    if (icodre(1) .eq. 1) call u2mesk('F', 'POSTRCCM_7', 1, 'RCCM')
!
    nocmp(1) = 'E'
    nocmp(2) = 'NU'
    nocmp(3) = 'ALPHA'
    nocmp(4) = 'E_REFE'
    nocmp(5) = 'SM'
    nocmp(6) = 'M_KE'
    nocmp(7) = 'N_KE'
!
! --- ON STOCKE 7 VALEURS : E, NU, ALPHA, E_REFE, SM, M_KE, N_KE
!     POUR LES 2 ETATS STABILISES DE CHAQUE SITUATION
!
    ndim = nbcmp2 * nbsitu
    call wkvect('&&RC3200.MATERIAU_A', 'V V R8', ndim, jvala)
    call wkvect('&&RC3200.MATERIAU_B', 'V V R8', ndim, jvalb)
!
    do 10, iocc = 1, nbsitu, 1
!
! ------ ETAT STABILISE "A"
!        ------------------
!
    call getvr8(motclf, 'TEMP_REF_A', iocc, iarg, 1,&
                tempa, na)
    if (na .eq. 0) then
        nbpa = 0
        nopa = ' '
        tempa = 0.d0
    else
        nbpa = 1
        nopa = 'TEMP'
    endif
!
    call rcvale(mater, 'ELAS', nbpa, nopa, tempa,&
                3, nocmp(1), para(1), icodre, 2)
!
    call rcvale(mater, 'FATIGUE', nbpa, nopa, tempa,&
                1, nocmp(4), para(4), icodre, 2)
!
    call rcvale(mater, 'RCCM', nbpa, nopa, tempa,&
                3, nocmp(5), para(5), icodre, 2)
!
    do 12 i = 1, nbcmp
        zr(jvala-1+nbcmp2*(iocc-1)+i) = para(i)
12  continue
    zr(jvala-1+nbcmp2*(iocc-1)+8) = tke
!
! ------ ETAT STABILISE "B"
!        ------------------
!
    call getvr8(motclf, 'TEMP_REF_B', iocc, iarg, 1,&
                tempb, nb)
    if (na .eq. 0) then
        nbpb = 0
        nopb = ' '
        tempb = 0.d0
    else
        nbpb = 1
        nopb = 'TEMP'
    endif
!
    call rcvale(mater, 'ELAS', nbpb, nopb, tempb,&
                3, nocmp(1), para(1), icodre, 2)
!
    call rcvale(mater, 'FATIGUE', nbpb, nopb, tempb,&
                1, nocmp(4), para(4), icodre, 2)
!
    call rcvale(mater, 'RCCM', nbpb, nopb, tempb,&
                3, nocmp(5), para(5), icodre, 2)
!
    do 14 i = 1, nbcmp
        zr(jvalb-1+nbcmp2*(iocc-1)+i) = para(i)
14  continue
    zr(jvalb-1+nbcmp2*(iocc-1)+8) = tke
!
    10 end do
!
    call jedema()
end subroutine
