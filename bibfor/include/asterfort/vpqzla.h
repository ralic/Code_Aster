!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
          interface
            subroutine vpqzla(typeqz,qrn,iqrn,lqrn,qrar,qrai,qrba,qrvl, &
     &lvec,kqrn,lvalpr,nconv,omecor,ktyp,kqrnr,neqact,ilscal,irscal,    &
     &optiof,omemin,omemax,omeshi,ddlexc,nfreq,lmasse,lraide,lamor,     &
     &numedd,sigma,icscal,ivscal,iiscal,bwork,flage)
              character(len=16) :: typeqz
              integer :: qrn
              integer :: iqrn
              integer :: lqrn
              integer :: qrar
              integer :: qrai
              integer :: qrba
              integer :: qrvl
              integer :: lvec
              integer :: kqrn
              integer :: lvalpr
              integer :: nconv
              real(kind=8) :: omecor
              character(len=1) :: ktyp
              integer :: kqrnr
              integer :: neqact
              integer :: ilscal
              integer :: irscal
              character(len=16) :: optiof
              real(kind=8) :: omemin
              real(kind=8) :: omemax
              real(kind=8) :: omeshi
              integer :: ddlexc(*)
              integer :: nfreq
              integer :: lmasse
              integer :: lraide
              integer :: lamor
              character(len=19) :: numedd
              complex(kind=8) :: sigma
              integer :: icscal
              integer :: ivscal
              integer :: iiscal
              logical(kind=4) :: bwork(*)
              logical(kind=1) :: flage
            end subroutine vpqzla
          end interface
