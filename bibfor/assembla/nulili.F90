subroutine nulili(nb_ligr, list_ligr, lili, base , gran_name,&
                  igds   , mesh     , nec , nlili, modelocz)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/nbgrel.h"
#include "asterfort/typele.h"
!
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
! person_in_charge: jacques.pellet at edf.fr
!
    integer, intent(in) :: nb_ligr
    character(len=24), pointer, intent(in) :: list_ligr(:)
    character(len=24), intent(in):: lili
    character(len=1), intent(in):: base
    character(len=8), intent(out) :: gran_name
    integer, intent(out) :: igds
    character(len=8), intent(out) :: mesh
    integer, intent(out) :: nec
    integer, intent(out) :: nlili
    character(len=*), optional, intent(in) :: modelocz
!
! --------------------------------------------------------------------------------------------------
!
! Factor
!
! Numbering - Create LILI objects
!
! --------------------------------------------------------------------------------------------------
!
! In  list_ligr      : pointer to list of LIGREL
! In  nb_ligr        : number of LIGREL in list
! In  lili           : name of LILI object to create
! In  base           : JEVEUX base to create LILI object
! Out gran_name      : name of GRANDEUR to number
! Out igds           : index of GRANDEUR to number
! Out mesh           : name of mesh
! Out nec            : number of coding integers for GRANDEUR
! Out nlili          : size of LILI object
! In  modelocz       : local mode
!
!----------------------------------------------------------------------
! ATTENTION : NE PAS FAIRE JEMARQ/JEDEMA CAR CETTE ROUTINE
!             RECOPIE DES ADRESSES JEVEUX DANS .ADNE ET .ADLI
!----------------------------------------------------------------------
!
!
!    --- DESCRIPTION DES OBJETS ADNE ET ADLI ---
!     ADNE (1          ) = NBRE DE MAILLES DU MAILLAGE
!     ADNE (2          ) = 0
!     ADNE (3          ) = 0
!     ADLI (1          ) = 0
!     ADLI (2          ) = 0
!     ADLI (3          ) = 0
!     POUR 2<=ILI<=NLILI
!     ADNE (3*(ILI-1)+1) = NBRE MAX D'OBJETS DE LA COLLECTION
!                            LILI(ILI).NEMA
!     ADNE (3*(ILI-1)+2) = ADRESSE DE L'OBJET LILI(ILI).NEMA
!     ADNE (3*(ILI-1)+3) = ADRESSE DU VECTEUR DES LONG. CUMULEES DE
!                            LILI(ILI).NEMA
!     ADLI (3*(ILI-1)+1) = NBRE MAX D'OBJETS DE LA COLLECTION
!                            LILI(ILI).LIEL
!     ADLI (3*(ILI-1)+2) = ADRESSE DE L'OBJET LILI(ILI).LIEL
!     ADLI (3*(ILI-1)+3) = ADRESSE DU VECTEUR DES LONG. CUMULEES DE
!                            LILI(ILI).LIEL
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: new_mesh, exiele
    character(len=16) :: phenom, new_phenom, nomte
    character(len=19) :: prefix
    character(len=24) :: ligr_name
    integer :: iad, i_grel, nb_elem
    integer :: i_list_ligr, iret
    integer :: nbgr, nbsup, jmoloc, imode, i_type_elem
    character(len=8) :: modeloc
    integer, pointer :: adli(:) => null()
    integer, pointer :: adne(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!    call jemarq() FORBIDDEN !
!
    prefix = lili(1:14)
    nlili  = nb_ligr+1
    ASSERT(nlili.gt.1)
!
! - Local mode
!
    modeloc = ' '
    if (present(modelocz)) then
        modeloc = modelocz
    endif
!
! - Create main LILI repertory object
!
    call jecreo(lili, base//' N  K24')
    call jeecra(lili, 'NOMMAX', nlili)
    call jecroc(jexnom(lili, '&MAILLA'))
!
! - Creation of temporary objects ADNE et ADLI
!
    call jecreo(prefix//'.ADNE', 'V V I')
    call jeecra(prefix//'.ADNE', 'LONMAX', 3*nlili)
    call jeveuo(prefix//'.ADNE', 'E', vi=adne)
    call jecreo(prefix//'.ADLI', 'V V I')
    call jeecra(prefix//'.ADLI', 'LONMAX', 3*nlili)
    call jeveuo(prefix//'.ADLI', 'E', vi=adli)
!
! - Save temporary objects ADNE et ADLI
!
    do i_list_ligr = 1, nb_ligr
        
        ligr_name = list_ligr(i_list_ligr)
!
! ----- Only one phenomenon
!
        call dismoi('PHENOMENE', ligr_name, 'LIGREL', repk=new_phenom)
        if (i_list_ligr .eq. 1) then
            phenom = new_phenom
        else
            ASSERT(phenom.eq.new_phenom)
        endif
!
! ----- Only one mesh
!
        call jeveut(ligr_name(1:19)//'.LGRF', 'L', iad)
        new_mesh = zk8(iad)
        if (i_list_ligr .eq. 1) then
            mesh = new_mesh 
        else
            ASSERT(mesh.eq.new_mesh)
        endif
!
! ----- Create object in collection
!
        call jecroc(jexnom(lili, ligr_name))
!
! ----- Set ADNE/ADLI objects
!
        call dismoi('EXI_ELEM', ligr_name, 'LIGREL', repk=exiele)
        if (exiele(1:3) .ne. 'NON') then
            call jeexin(ligr_name(1:19)//'.NEMA', iret)
            if (iret .ne. 0) then
!
!---- ADNE(3*(i_list_ligr)+1)=NBRE DE MAILLES SUP DU LIGREL NOMLI
!
                call jelira(ligr_name(1:19)//'.NEMA', 'NUTIOC', nbsup)
                adne(1+3*(i_list_ligr)) = nbsup
                call jeveut(ligr_name(1:19)//'.NEMA', 'L', iad)
                adne(1+3*(i_list_ligr)+1) = iad
                call jeveut(jexatr(ligr_name(1:19)//'.NEMA', 'LONCUM'), 'L', iad)
                adne(1+3*(i_list_ligr)+2) = iad
            else
                adne(1+3* (i_list_ligr)) = 0
                adne(1+3* (i_list_ligr)+1) = 2**30
                adne(1+3* (i_list_ligr)+2) = 2**30
            endif
!
!---- ADLI(3*(i_list_ligr)+1)=NBRE DE MAILLES DU LIGREL NOMLI
!
            call jelira(ligr_name(1:19)//'.LIEL', 'NUTIOC', nbgr)
            adli(1+3* (i_list_ligr)) = nbgr
            call jeveut(ligr_name(1:19)//'.LIEL', 'L', iad)
            adli(1+3* (i_list_ligr)+1) = iad
            call jeveut(jexatr(ligr_name(1:19)//'.LIEL', 'LONCUM'), 'L', iad)
            adli(1+3* (i_list_ligr)+2) = iad
        endif
    end do
    call dismoi('NB_MA_MAILLA', mesh(1:8), 'MAILLAGE', repi=nb_elem)
    adne(1) = nb_elem
!
! - Information about GRANDEUR
!
    if (modeloc .eq. ' ') then
        call dismoi('NOM_GD', phenom, 'PHENOMENE', repk=gran_name)
    else
        ligr_name = list_ligr(1)(1:19)
        do i_grel = 1, nbgrel(ligr_name)
            i_type_elem = typele(ligr_name,i_grel)
            call jenuno(jexnum('&CATA.TE.NOMTE', i_type_elem), nomte)
            call jenonu(jexnom('&CATA.TE.NOMMOLOC', nomte//modeloc), imode)
            if (imode .gt. 0) then
                call jeveuo(jexnum('&CATA.TE.MODELOC', imode), 'L', jmoloc)
                call jenuno(jexnum('&CATA.GD.NOMGD', zi(jmoloc-1+2)), gran_name)
                goto 30
            endif
        end do
        ASSERT(.false.)
 30     continue
    endif
    call jenonu(jexnom('&CATA.GD.NOMGD', gran_name), igds)
    ASSERT(igds.ne.0)
!
    nec  = nbec(igds)
!
!    call jedema() FORBIDDEN !
!
end subroutine
