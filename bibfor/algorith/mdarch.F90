subroutine mdarch(isto1, ipas, disc, dt, nbmode,&
                  typcal, nbsym, nomsym, depger, vitger,&
                  accger, depstr, vitstr, accstr, depgec,&
                  vitgec, accgec, depstc, vitstc, accstc,&
                  passto, iorsto, discst)
    implicit none
    include 'asterfort/assert.h'
    integer :: iorsto(*)
    real(kind=8) :: depger(*), vitger(*), accger(*), depstr(*), vitstr(*)
    real(kind=8) :: accstr(*), passto(*), discst(*)
    complex(kind=8) :: depgec(*), vitgec(*), accgec(*), depstc(*), vitstc(*)
    complex(kind=8) :: accstc(*)
    character(len=4) :: typcal
    character(len=4) :: nomsym(3)
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_21
!
!-----------------------------------------------------------------------
!
!   ARCHIVAGE DES CHAMPS GENERALISES OBLIGATOIRES POUR LA SD_DYNA_GENE
!
!-----------------------------------------------------------------------
    integer :: im, ind, ipas, isto1, nbsym, ich
    integer :: nbmode
    real(kind=8) :: dt, disc
!-----------------------------------------------------------------------
!
    call assert((typcal(1:4).eq.'TRAN').or.(typcal(1:4).eq.'HARM'))
!
    iorsto(isto1+1) = ipas
    discst(isto1+1) = disc
    ind = nbmode * isto1
!
    if (typcal(1:4) .eq. 'TRAN') then
!
        passto(isto1+1) = dt
        do 69 im = 1, nbmode
            depstr(ind+im) = depger(im)
            vitstr(ind+im) = vitger(im)
            accstr(ind+im) = accger(im)
69      continue
!
    else
!
        do 100 ich = 1, nbsym
!
            if (nomsym(ich)(1:4) .eq. 'DEPL') then
                do 101 im = 1, nbmode
                    depstc(ind+im) = depgec(im)
101              continue
            else if (nomsym(ich)(1:4).eq.'VITE') then
                do 102 im = 1, nbmode
                    vitstc(ind+im) = vitgec(im)
102              continue
            else
                do 103 im = 1, nbmode
                    accstc(ind+im) = accgec(im)
103              continue
            endif
!
100      continue
!
    endif
!
end subroutine
