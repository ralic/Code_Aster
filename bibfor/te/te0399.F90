subroutine te0399(option, nomte)

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
! person_in_charge: mohamed.torkhani at edf.fr

    implicit none
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/arlref.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/arlapl.h"
#include "asterfort/jedema.h"

    character(len=16) :: nomte,option

! ----------------------------------------------------------------------

! CALCUL DES MATRICES DE COUPLAGE ARLEQUIN
! OPTION ARLQ_MATR

! CREATION DES 2 MATRICES DE COUPLAGE POUR LES ELEMENTS 1D ET 3D
!

! ----------------------------------------------------------------------


! IN  OPTION : OPTION DE CALCUL
! IN  NOMTE  : NOM DU TYPE ELEMENT


    integer :: nbnomx
    parameter (nbnomx = 27)
    integer :: nbgamx
    parameter (nbgamx = 64)

    integer :: nns,nnos
    integer :: npgs,ipoids,ivfs,idfdes,jrefe1,jrefe2
    integer :: jfamil,jinfor,jcoopg,jdfd2,jgano
    integer :: ndim
    character(len=8) :: nomfam,elrfs
    character(len=8) :: elrf1,elrf2
    character(len=16) :: nomte1,nomte2
    integer :: nn1,nn2

! ----------------------------------------------------------------------
    call jemarq()

! --- FAMILLE D'INTEGRATION

    call jevech('PFAMILK','L',jfamil)
    nomfam = zk8(jfamil)

! --- INFORMATIONS SUR MAILLES COUPLEES

    call jevech('PINFORR','L',jinfor)
    ndim   = nint(zr(jinfor+1-1))
    nn1    = nint(zr(jinfor+2-1))
    nn2    = nint(zr(jinfor+3-1))
    call jevech('PREFE1K','L',jrefe1)
    elrf1 = zk8(jrefe1)
    call jevech('PREFE2K','L',jrefe2)
    elrf2 = zk8(jrefe2)

! --- SCHEMA INTEGRATION MAILLE SUPPORT

    call elref1(elrfs)

    if (elrf2 == 'SE2') then
        nomte2 = 'MECA_POU_D_T'
    endif
    if (elrf1 == 'H20') then
        nomte1 = 'MECA_HEXA20'
    elseif (elrf1 == 'HE8') then
        nomte1 = 'MECA_HEXA8'
    elseif (elrf1 == 'P15') then
        nomte1 = 'MECA_PENTA15'
    elseif (elrf1 == 'PE6') then
        nomte1 = 'MECA_PENTA6'
    elseif (elrf1 == 'T10') then
        nomte1 = 'MECA_TETRA10'
    elseif (elrf1 == 'TE4') then
        nomte1 = 'MECA_TETRA4'
    endif

    if (nomte1 .ne. nomte) then
        call arlref(elrefe=elrf1,fami=nomfam,nomte=nomte1,ndim=ndim,nno=nns,nnos=nnos,&
                    npg=npgs,jpoids=ipoids,jcoopg=jcoopg,jvf=ivfs,jdfde=idfdes,&
                    jdfd2=jdfd2,jgano=jgano)
        nns = nn1
    else
        call elrefe_info(elrefe=elrfs,fami=nomfam,ndim=ndim,nno=nns,nnos=nnos,&
                         npg=npgs,jpoids=ipoids,jcoopg=jcoopg,jvf=ivfs,jdfde=idfdes,&
                         jdfd2=jdfd2,jgano=jgano)
    endif

! --- VERIFICATIONS

    ASSERT(nn1.le.nbnomx)
    ASSERT(nn2.le.nbnomx)
    ASSERT(nns.le.nbnomx)
    ASSERT(npgs.le.nbgamx)

    call arlapl(ndim,nns,nn1,nn2,nomte,npgs,ipoids,ivfs,idfdes)

    call jedema()

end subroutine
