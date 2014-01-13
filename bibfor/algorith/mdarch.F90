subroutine mdarch(typcal, isto1, ipas, disc, nbmode,&
                  iorsto, discst, dt, depger, vitger,&
                  accger, depstr, vitstr, accstr, passto,&
                  nbsym, nomsym, depgec, vitgec, accgec,&
                  depstc, vitstc, accstc)

! aslint: disable=W1504
    implicit none
#include "asterfort/assert.h"
!
!   Obligatory arguments
    character(len=4) , intent(in)  :: typcal
    integer          , intent(in)  :: isto1
    integer          , intent(in)  :: ipas
    real(kind=8)     , intent(in)  :: disc
    integer          , intent(in)  :: nbmode
    integer                        :: iorsto(*)
    real(kind=8)                   :: discst(*)
!
!   Optional arguments
!     typcal = 'TRAN' case
    real(kind=8)     , optional, intent(in)  :: dt
    real(kind=8)     , optional, intent(in)  :: depger(nbmode)
    real(kind=8)     , optional, intent(in)  :: vitger(nbmode)
    real(kind=8)     , optional, intent(in)  :: accger(nbmode)
    real(kind=8)     , optional              :: depstr(*)
    real(kind=8)     , optional              :: vitstr(*)
    real(kind=8)     , optional              :: accstr(*)
    real(kind=8)     , optional              :: passto(*)
!     typcal = 'HARM' case
    integer          , optional, intent(in)  :: nbsym
    character(len=4) , optional, intent(in)  :: nomsym(*)
    complex(kind=8)  , optional, intent(in)  :: depgec(nbmode)
    complex(kind=8)  , optional, intent(in)  :: vitgec(nbmode)
    complex(kind=8)  , optional, intent(in)  :: accgec(nbmode)
    complex(kind=8)  , optional              :: depstc(*)
    complex(kind=8)  , optional              :: vitstc(*)
    complex(kind=8)  , optional              :: accstc(*)
! ----------------------------------------------------------------------
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
!
!-----------------------------------------------------------------------
!
!   ARCHIVAGE DES CHAMPS GENERALISES OBLIGATOIRES POUR LA SD_DYNA_GENE
!
!-----------------------------------------------------------------------
    integer :: im, ind, ich, inom, nbsym2
    character(len=4) :: nomsym2(3)
!-----------------------------------------------------------------------
!
    nbsym2 = 3
    nomsym2 = ['DEPL','VITE','ACCE']
    ASSERT(ENSEMBLE2(nomsym,nbsym))
    if (present(nbsym)) then
        nbsym2 = nbsym
        do inom = 1, nbsym2
            nomsym2(inom) = nomsym(inom)
        end do
    endif

    ASSERT((typcal(1:4).eq.'TRAN').or.(typcal(1:4).eq.'HARM'))
    if (typcal.eq.'TRAN') then
        ASSERT(present(dt)    .and.present(depger).and.present(vitger))
        ASSERT(present(accger).and.present(depstr).and.present(vitstr))
        ASSERT(present(accstr).and.present(passto))
        ASSERT(absent(nbsym).and.absent(nomsym))
    else
        ASSERT(present(depgec).and.present(vitgec).and.present(accgec))
        ASSERT(present(depstc).and.present(vitstc).and.present(accstc))
        ASSERT(absent(dt).and.absent(passto))
    endif
!
    iorsto(isto1+1) = ipas
    discst(isto1+1) = disc
    ind = nbmode * isto1
!
    if (typcal(1:4) .eq. 'TRAN') then
!
        passto(isto1+1) = dt
        do im = 1, nbmode
            depstr(ind+im) = depger(im)
            vitstr(ind+im) = vitger(im)
            accstr(ind+im) = accger(im)
        enddo
    else
        do ich = 1, nbsym
            if (nomsym(ich)(1:4) .eq. 'DEPL') then
                do im = 1, nbmode
                    depstc(ind+im) = depgec(im)
                enddo
            else if (nomsym(ich)(1:4).eq.'VITE') then
                do im = 1, nbmode
                    vitstc(ind+im) = vitgec(im)
                enddo
            else
                do im = 1, nbmode
                    accstc(ind+im) = accgec(im)
                enddo
            endif
        enddo
!
    endif
!
end subroutine
