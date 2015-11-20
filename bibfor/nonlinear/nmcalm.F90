subroutine nmcalm(typmat, modelz, lischa, mate      , carele,&
                  compor, instam, instap, valinc    , solalg,&
                  optmaz, base  , meelem, ds_contact,&
                  matele)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/meamme.h"
#include "asterfort/mecgm2.h"
#include "asterfort/mecgme.h"
#include "asterfort/medime.h"
#include "asterfort/memame.h"
#include "asterfort/memare.h"
#include "asterfort/merige.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/nmelcm.h"
#include "asterfort/wkvect.h"
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
    character(len=*) :: modelz
    character(len=*) :: mate, carele
    character(len=24) :: compor
    real(kind=8) :: instam, instap
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: lischa
    character(len=6) :: typmat
    character(len=*) :: optmaz
    character(len=1) :: base
    character(len=19) :: meelem(*), solalg(*), valinc(*)
    character(len=19) :: matele
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES MATRICES ELEMENTAIRES
!
! ----------------------------------------------------------------------
!
! IN  MODELE : NOM DU MODELE
! IN  LISCHA : LISTE DES CHARGEMENTS
! IN  MATE   : CHAMP MATERIAU
! In  ds_contact       : datastructure for contact management
! IN  TYPMAT : TYPE DE MATRICE A CALCULER
!                MERIGI  - MATRICE POUR RIGIDITE
!                MEDIRI  - MATRICE POUR CL DIRICHLET LAGRANGE
!                MEGEOM  - MATRICE POUR NON-LIN. GEOMETRIQUE
!                MEAMOR  - MATRICE POUR AMORTISSEMENT
!                MEMASS  - MATRICE POUR MASSE
!                MESUIV  - MATRICE POUR CHARGEMENT SUIVEUR
!                MESSTR  - MATRICE POUR SOUS-STRUCTURES
!                MEELTC  - MATRICE POUR ELTS DE CONTACT
!                MEELTF  - MATRICE POUR ELTS DE FROTTEMENT
! IN  OPTCAL : OPTION DE CALCUL DU MATR_ELEM
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  OPTMAT : OPTION DE CALCUL POUR LA MATRICE
! OUT MATELE : MATRICE ELEMENTAIRE
!
!
!
!
    character(len=19) :: memass, merigi
    character(len=24) :: model
    integer :: jinfc, jchar, jchar2
    integer :: nbchar
    integer :: i
    character(len=16) :: optmat
    character(len=19) :: disp_prev, sigplu, vite_curr, vite_prev, acce_prev, strplu
    character(len=19) :: disp_cumu_inst, varplu
    character(len=24) :: charge, infoch
    character(len=8) :: mesh
    integer :: ifm, niv
    character(len=24), pointer :: rerr(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    optmat = optmaz
    model = modelz
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><MATR> CALCUL DES MATR_ELEM DE TYPE <',typmat,'>'
    endif
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    if (valinc(1)(1:1) .ne. ' ') then
        call nmchex(valinc, 'VALINC', 'DEPMOI', disp_prev)
        call nmchex(valinc, 'VALINC', 'VITMOI', vite_prev)
        call nmchex(valinc, 'VALINC', 'ACCMOI', acce_prev)
        call nmchex(valinc, 'VALINC', 'VITPLU', vite_curr)
        call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
        call nmchex(valinc, 'VALINC', 'STRPLU', strplu)
        call nmchex(valinc, 'VALINC', 'VARMOI', varplu)
    endif
    if (solalg(1)(1:1) .ne. ' ') then
        call nmchex(solalg, 'SOLALG', 'DEPDEL', disp_cumu_inst)
    endif
    if (meelem(1)(1:1) .ne. ' ') then
        call nmchex(meelem, 'MEELEM', 'MERIGI', merigi)
        call nmchex(meelem, 'MEELEM', 'MEMASS', memass)
    endif
!
! --- TRANSFO CHARGEMENTS
!
    charge = lischa(1:19)//'.LCHA'
    infoch = lischa(1:19)//'.INFC'
    call jeveuo(infoch, 'L', jinfc)
    nbchar = zi(jinfc)
    if (nbchar .ne. 0) then
        call jeveuo(charge, 'L', jchar)
        call wkvect('&&NMCALC.LISTE_CHARGE', 'V V K8', nbchar, jchar2)
        do i = 1, nbchar
            zk8(jchar2-1+i) = zk24(jchar-1+i) (1:8)
        end do
    else
        call wkvect('&&NMCALC.LISTE_CHARGE', 'V V K8', 1, jchar2)
    endif
!
    if (typmat .eq. 'MEDIRI') then
!
! --- MATR_ELEM DES CL DE DIRICHLET B
!
        call medime('V', 'ZERO', model, lischa, matele)
!
! --- MATR_ELEM RIGIDITE GEOMETRIQUE
!
    else if (typmat.eq.'MEGEOM') then
        call detrsd('MATR_ELEM', matele)
        call merige(model(1:8), carele(1:8), sigplu, strplu, matele,&
                    'V', 0)
!
! --- MATR_ELEM MASSES
!
    else if (typmat.eq.'MEMASS') then
        call memame(optmat, model, nbchar, zk8(jchar2), mate,&
                    carele, .true._1, instam, compor, matele,&
                    base)
!
! --- MATR_ELEM AMORTISSEMENT
!
    else if (typmat.eq.'MEAMOR') then
        call meamme(optmat, model, nbchar, zk8(jchar2), mate,&
                    carele, .true._1, instam, 'V', merigi,&
                    memass, matele, varplu)
!
! --- MATR_ELEM POUR CHARGES SUIVEUSES
!
    else if (typmat.eq.'MESUIV') then
        call mecgme(model, carele, mate  , lischa, instap,&
                    disp_prev, disp_cumu_inst, instam, compor, matele)
        call mecgm2(lischa, instap, matele)
!
! --- MATR_ELEM DES SOUS-STRUCTURES
!
    else if (typmat.eq.'MESSTR') then
        call memare(base, matele, model(1:8), mate, carele,&
                    optmat)
        call jeveuo(matele//'.RERR', 'E', vk24=rerr)
        rerr(3) = 'OUI_SOUS_STRUC'
!
! --- MATR_ELEM DES ELTS DE CONTACT (XFEM+CONTINUE)
!
    else if (typmat.eq.'MEELTC') then
        call nmelcm('CONT'   , mesh     , model    , mate     , ds_contact    ,&
                    disp_prev, vite_prev, acce_prev, vite_curr, disp_cumu_inst,&
                    matele)
!
! --- MATR_ELEM DES ELTS DE FROTTEMENT (XFEM+CONTINUE)
!
    else if (typmat.eq.'MEELTF') then
        call nmelcm('FROT'   , mesh     , model    , mate     , ds_contact    ,&
                    disp_prev, vite_prev, acce_prev, vite_curr, disp_cumu_inst,&
                    matele)
    else
        ASSERT(.false.)
    endif
!
! --- DEBUG
!
    if (niv .eq. 2) then
        call nmdebg(' ', matele, ifm)
    endif
!
! --- MENAGE
!
    call jedetr('&&NMCALC.LISTE_CHARGE')
    call jedema()
!
end subroutine
