!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
!
interface 
    subroutine xpoajm(maxfem, jtypm2, itypse, jcnse, im,&
                      n, nnose, prefno, jdirno, nnm,&
                      inm, inmtot, nbmac, he, jnivgr,&
                      iagma, ngrm, jdirgr, opmail, nfiss,&
                      ndim, ndime, jconx1, jconx2, jconq1,&
                      jconq2, ima, iad1, nnn, inn,&
                      inntot, nbnoc, nbnofi, inofi, iacoo1,&
                      iacoo2, iad9, ninter, iainc, ncompa, elrefp,&
                      jlsn, jlst, typma, igeom, jheavn, ncompn,&
                      contac, cmp, nbcmp, nfh, nfe,&
                      ddlc, jcnsv1, jcnsv2, jcnsl2, lmeca,&
                      pre1, heavno, fisco,&
                      nlachm, lacthm, jbaslo, jstno, ka, mu)
        integer :: nfiss
        character(len=8) :: maxfem
        integer :: jtypm2
        integer :: itypse
        integer :: jcnse
        integer :: im
        integer :: n
        integer :: nnose
        character(len=2) :: prefno(4)
        integer :: jdirno
        integer :: nnm
        integer :: inm
        integer :: inmtot
        integer :: nbmac
        integer :: he(nfiss)
        integer :: jnivgr
        integer :: iagma
        integer :: ngrm
        integer :: jdirgr
        aster_logical :: opmail
        integer :: ndim
        integer :: ndime
        integer :: jconx1
        integer :: jconx2
        integer :: jconq1
        integer :: jconq2
        integer :: ima
        integer :: iad1
        integer :: nnn
        integer :: inn
        integer :: inntot
        integer :: nbnoc
        integer :: nbnofi
        integer :: inofi
        integer :: iacoo1
        integer :: iacoo2
        integer :: iad9
        integer :: ninter(4)
        integer :: iainc
        character(len=8) :: elrefp
        integer :: jlsn
        integer :: jlst
        character(len=8) :: typma
        integer :: igeom
        integer :: jheavn
        integer :: ncompn
        integer :: contac
        integer :: cmp(*)
        integer :: nbcmp
        integer :: nfh
        integer :: nfe
        integer :: ddlc
        integer :: jcnsv1
        integer :: jcnsv2
        integer :: jcnsl2
        integer :: jstno
        aster_logical :: lmeca
        aster_logical :: pre1
        integer :: ncompa
        integer :: heavno(20,3)
        integer :: fisco(*)
        integer :: nlachm(2)
        integer :: lacthm(16)
        integer :: jbaslo
        real(kind=8) :: ka
        real(kind=8) :: mu
    end subroutine xpoajm
end interface 
