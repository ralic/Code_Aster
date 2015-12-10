subroutine cfcgeo(mesh, ds_contact, solalg, dvgeom,&
                  geoerr, geonoe, geoval)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfverl.h"
#include "asterfort/cnomax.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmbouc.h"
#include "asterfort/nmchex.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
    aster_logical :: dvgeom, geoerr
    character(len=19) :: solalg(*)
    character(len=16) :: geonoe
    real(kind=8) :: geoval
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES)
!
! CONVERGENCE LIEE AU CONTACT DISCRET - GEOMETRIE
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! IN  NOMA   : NOM DU MAILLAGE
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT DVGEOM : .TRUE. SI BOUCLE GEOMETRIQUE NON CONVERGEE
! OUT GEOERR : .TRUE. SI ERREUR SUR LA BOUCLE DE GEOMETRIE
! OUT GEONOE : ENDROIT POUR MAX GEOMETRIE
! OUT GEOVAL : VALEUR DU MAX GEOMETRIE
!
!
!
!
    integer :: ii, numno1, numno2
    integer :: neq
    integer :: ncmp, mmitgo, nbreag, maxgeo
    real(kind=8) :: autono, temp1, temp2, epsgeo, rmin
    character(len=8) :: nomnoe, licmp(3)
    aster_logical :: premie, alarme
    character(len=19) :: depdel
    character(len=24) :: maxdep, autoc1, autoc2
    integer :: jmaxde
    aster_logical :: geoala
    real(kind=8), pointer :: auto1(:) => null()
    real(kind=8), pointer :: auto2(:) => null()
    real(kind=8), pointer :: depde(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
!
! --- ACCES OBJETS
!
    autoc1 = ds_contact%sdcont_solv(1:14)//'.REA1'
    autoc2 = ds_contact%sdcont_solv(1:14)//'.REA2'
    maxdep = ds_contact%sdcont_solv(1:14)//'.MAXD'
    call jeveuo(depdel(1:19)//'.VALE', 'L', vr=depde)
    call jeveuo(autoc1(1:19)//'.VALE', 'E', vr=auto1)
    call jeveuo(autoc2(1:19)//'.VALE', 'E', vr=auto2)
    call jeveuo(maxdep, 'E', jmaxde)
!
! --- TOLERANCE POUR REACTUALISATION GEOMETRIQUE AUTOMATIQUE
! --- NOMBRE MAXI D'ITERATIONS DE REACTUALISATION GEOMETRIQUE EN AUTO
!
    epsgeo = cfdisr(ds_contact%sdcont_defi,'RESI_GEOM' )
    nbreag = cfdisi(ds_contact%sdcont_defi,'NB_ITER_GEOM' )
    maxgeo = cfdisi(ds_contact%sdcont_defi,'ITER_GEOM_MAXI')
    neq = cfdisd(ds_contact%sdcont_solv,'NEQ' )
    geoala = .false.
    geoerr = .false.
    dvgeom = .true.
    premie = .false.
    alarme = .false.
    geonoe = ' '
    geoval = r8vide()
    temp1 = 0.d0
    temp2 = 0.d0
!
! --- NOUVELLE ITERATION DE REACTUALISATION GEOMETRIQUE
!
    call mmbouc(ds_contact, 'GEOM', 'READ', mmitgo)
!
! --- CALCUL DU DEPLACEMENT
!
    do ii = 1, neq
        auto2(ii) = auto2(ii) + auto1(ii)
        auto1(ii) = depde(ii) - auto2(ii)
    end do
!
! --- CALCUL DU MAX DE LA NORME DU DEPLACEMENT (SAUF LAGRANGES)
!
    ncmp = 3
    licmp(1) = 'DX'
    licmp(2) = 'DY'
    licmp(3) = 'DZ'
    call cnomax(autoc1, ncmp, licmp, temp1, numno1)
    call cnomax(autoc2, ncmp, licmp, temp2, numno2)
!
! --- STOCKAGE DU MAX DE LA NORME DU DEPL
!
    if (zr(jmaxde) .lt. 0.d0) then
        zr(jmaxde-1+1) = temp2
        rmin = r8prem()
    else
        zr(jmaxde-1+1) = max(zr(jmaxde-1+1),temp2)
        rmin = 1.d-6*zr(jmaxde-1+1)
    endif
!
! --- VALEUR DE DEPL. MAX MESURE
!
    if (temp2 .le. rmin) then
        if (temp2 .eq. 0.d0) then
            autono = 10.0d0*epsgeo
            premie = .true.
        else
            autono = 1.d-1*epsgeo
        endif
    else
        autono = temp1/temp2
    endif
!
! --- INFORMATIONS: NOM DU NOEUD ET VALEUR REACTUALISATION
!
    if (numno2 .eq. 0) then
        nomnoe = ' '
    else
        call jenuno(jexnum(mesh//'.NOMNOE', numno2), nomnoe)
    endif
    geonoe = nomnoe//'        '
    geoval = autono
!
! --- CORRESPOND A REAC_GEOM = AUTOMATIQUE
!
    if (cfdisl(ds_contact%sdcont_defi,'REAC_GEOM_AUTO')) then
!
        if (autono .lt. epsgeo) then
            dvgeom = .false.
        else
            dvgeom = .true.
            if (mmitgo .eq. maxgeo) then
                call cfverl(ds_contact)
                geoerr = .true.
            endif
        endif
!
! --- CORRESPOND A REAC_GEOM = SANS
!
    else if (cfdisl(ds_contact%sdcont_defi,'REAC_GEOM_SANS')) then
        if (autono .ge. epsgeo) then
            alarme = .true.
        endif
        dvgeom = .false.
    else if (cfdisl(ds_contact%sdcont_defi,'REAC_GEOM_MANU')) then
!
! --- CORRESPOND A REAC_GEOM = CONTROLE
!
        if (mmitgo .eq. nbreag) then
            dvgeom = .false.
            if (autono .ge. epsgeo) then
                alarme = .true.
            endif
        else
            dvgeom = .true.
        endif
    else
        ASSERT(.false.)
    endif
!
! --- SI ON DEPASSE LA TOLERANCE DE 5% DE REAC_GEOM ET SI ON N'EST
! --- PAS AU PREMIER PAS DE TEMPS OU QU'ON N'A PAS DE CORPS RIGIDE
!
    if (alarme .and. .not.premie) then
        geoala = .true.
    else
        geoala = .false.
    endif
!
! --- AFFICHAGE
!
    if (geoala) then
        call utmess('A', 'CONTACT3_96')
    endif
!
    call jedema()
end subroutine
