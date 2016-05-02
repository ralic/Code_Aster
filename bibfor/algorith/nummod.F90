subroutine nummod(nugene, modmec)
    implicit none
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nummo1.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
    character(len=8) :: modmec
    character(len=14) :: nugene
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!***********************************************************************
!    G. JACQUART     DATE 1995
!-----------------------------------------------------------------------
!    BUT: < NUMEROTATION GENERALISEE >
!
!    DETERMINER LA NUMEROTATION GENERALISEE A PARTIR D'UN MODE_MECA
!    OU D'UN MODE_GENE
!    LA NUMEROTATION SERA PAR DEFAUT PLEINE
!
! NUGENE /I/ : NOM K14 DU NUME_DDL_GENE
! MODMEC /I/ : NOM K8 DU MODE_MECA OU DU MODE_GENE
!-----------------------------------------------------------------------
!
    integer :: ibid, n1, nbvect, nbmode, tmod(1), n2
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    character(len=8) :: k8b, typrof
!-----------------------------------------------------------------------
!
    call jemarq()
!
    call getvis(' ', 'NB_VECT', scal=nbvect, nbret=n2)
    call getvtx(' ', 'STOCKAGE', scal=typrof, nbret=n1)
!
!-----RECUPERATION DU NB DE MODES DU CONCEPT MODE_MECA OU MODE_GENE
!
    call rsorac(modmec, 'LONUTI', ibid, rbid, k8b,&
                cbid, rbid, k8b, tmod, 1,&
                n1)
    nbmode=tmod(1)            
!
!-----TEST NBVECT A UTILISER / NBMODE
!
    if (n2 .eq. 0) then
        nbvect = nbmode
    endif
    if (nbvect .lt. nbmode) then
        nbmode = nbvect
        call utmess('A', 'ALGORITH9_8')
    elseif (nbvect .gt. nbmode) then
        call utmess('A', 'ALGORITH9_10')
    else
        nbmode = nbvect
    endif
    if (nbmode .eq. 0) then
        call utmess('F', 'ALGORITH12_81')
    endif
!
    call nummo1(nugene, modmec, nbmode, typrof)
!
    call jedema()
end subroutine
