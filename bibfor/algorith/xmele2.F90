subroutine xmele2(mesh  , model, ds_contact, ligrel, nfiss,&
                  chelem)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/wkvect.h"
#include "asterfort/xxconi.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: nfiss
    character(len=19), intent(in) :: chelem
    character(len=19), intent(in) :: ligrel
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - CREATION CHAM_ELEM)
!
! CREATION DU CHAM_ELEM PDONCO (DONNEES DU CONTACT)
!
! ----------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  ds_contact       : datastructure for contact management
! IN  NFISS  : NOMBRE TOTAL DE FISSURES
! IN  LIGREL : NOM DU LIGREL DES MAILLES TARDIVES
! IN  CHELEM : NOM DU CHAM_ELEM CREEE
!
    integer, parameter :: nbcmp = 10
    character(len=8), parameter :: licmp(nbcmp) = (/ 'RHON  ','MU    ','RHOTK ','INTEG ',&
                                                     'COECH ','COSTCO','COSTFR','COPECO',&
                                                     'COPEFR','RELA  '/)
    integer :: ifm, niv, iret
    integer :: ibid, iad, i, ima, ifis, izone
    integer :: nmaenr, nbma,  ispt, icmp
    character(len=8) :: nomfis
    integer :: jcesl,  jcesd
    character(len=24) :: grp
    integer :: jgrp
    character(len=19) :: chelsi
    real(kind=8) :: coef(nbcmp)
    integer :: jmail, contac
    integer, pointer :: nbsp(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    character(len=8), pointer :: fiss(:) => null()
    integer, pointer :: xfem_cont(:) => null()

!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<XFEM  > CREATION DU CHAM_ELEM PDONCO'
    endif
!
! --- INITIALISATIONS
!
    chelsi = '&&XMELE2.CES'
    call jeveuo(model//'.XFEM_CONT','L',vi=xfem_cont)
    contac = xfem_cont(1)
!
! --- RECOPIE DU NOMBRE DE SOUS POINTS DE TOPOSE.HEA DANS LE CHAMP NBSP
!
    call celces(model//'.TOPOSE.HEA', 'V', '&&XMELE2.HEAV')
    call jeveuo('&&XMELE2.HEAV      .CESD', 'L', jcesd)
    call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi=nbma)
    AS_ALLOCATE(vi=nbsp, size=nbma)
    do ima = 1, nbma
        nbsp(ima) = zi(jcesd-1+5+4*(ima-1)+2)
    end do
!
! --- CREATION DU CHAM_ELEM_S
!
    call cescre('V', chelsi, 'ELEM', mesh, 'XCONTAC',&
                nbcmp, licmp, [-1], nbsp, [-nbcmp])
!
! --- ACCES AU CHAM_ELEM_S
!
    call jeveuo(chelsi//'.CESD', 'L', jcesd)
    call jeveuo(chelsi//'.CESL', 'E', jcesl)
    call jeveuo(chelsi//'.CESV', 'E', vr=cesv)
!
! --- ACCES AUX FISSURES
!
    call jeveuo(model//'.FISS', 'L', vk8=fiss)
!
! --- RECUPERATION DES MAILLES DU MODELE
    call jeveuo(model//'.MAILLE', 'L', jmail)
!
! --- ENRICHISSEMENT DU CHAM_ELEM POUR LA MULTIFISSURATION
!
    do ifis = 1, nfiss
!
! --- ACCES FISSURE COURANTE
!
        nomfis = fiss(ifis)
!
! --- INFORMATIONS SUR LA FISSURE
!
        grp = nomfis(1:8)//'.MAILFISS.CONT'
!
! --- ZONE DE CONTACT IZONE CORRESPONDANTE
!
        izone = xxconi(ds_contact%sdcont_defi,nomfis,'MAIT')
!
! --- CARACTERISTIQUES DU CONTACT POUR LA FISSURE EN COURS
!
        coef(1) = mminfr(ds_contact%sdcont_defi,'COEF_AUGM_CONT',izone )
        coef(2) = mminfr(ds_contact%sdcont_defi,'COEF_COULOMB' ,izone )
        coef(3) = mminfr(ds_contact%sdcont_defi,'COEF_AUGM_FROT',izone )
        coef(4) = mminfi(ds_contact%sdcont_defi,'INTEGRATION' ,izone )
        coef(6) = mminfr(ds_contact%sdcont_defi,'ALGO_CONT' ,izone )
        coef(7) = mminfr(ds_contact%sdcont_defi,'ALGO_FROT' ,izone )
        coef(8) = mminfr(ds_contact%sdcont_defi,'COEF_PENA_CONT',izone )
        coef(9) = mminfr(ds_contact%sdcont_defi,'COEF_PENA_FROT',izone )
        coef(10)= mminfr(ds_contact%sdcont_defi,'RELATION' ,izone )
!
! --- ON COPIE LES CHAMPS CORRESP. AUX ELEM. HEAV, CTIP ET HECT
!
        call jeexin(grp, iret)
        if (iret .ne. 0) then
            call jeveuo(grp, 'L', jgrp)
            call jelira(grp, 'LONMAX', nmaenr)
            do i = 1, nmaenr
                ima = zi(jgrp-1+i)
!
! --- RECUPERATION DU NUMÉRO DE SOUS POINT ISPT
!
                do ispt = 1, nbsp(ima)
                    call cesexi('S', jcesd, jcesl, ima, 1,&
                                ispt, 1, iad)
                    if (iad .lt. 0) goto 140
                end do
                ASSERT(.false.)
140             continue
!
! --- RECOPIE EFFECTIVE DES CHAMPS
!
                do icmp = 1, nbcmp
                    call cesexi('S', jcesd, jcesl, ima, 1,&
                                ispt, icmp, iad)
                    zl(jcesl-1-iad) = .true.
                    cesv(1-1-iad) = coef(icmp)
                end do
            end do
        endif
!
    end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
!
    if(contac.eq.1.or.contac.eq.3) then
        call cescel(chelsi, ligrel, 'XCVBCA', 'PDONCO', 'NON',&
                    ibid, 'V', chelem, 'F', ibid)
    else if(contac.eq.2) then
        call cescel(chelsi, ligrel, 'RIGI_CONT_M', 'PDONCO', 'NON',&
                    ibid, 'V', chelem, 'F', ibid)
    endif
!
! --- MENAGE
!
    call detrsd('CHAM_ELEM_S', chelsi)
    call detrsd('CHAM_ELEM_S', '&&XMELE2.HEAV')
    AS_DEALLOCATE(vi=nbsp)
!
    call jedema()
!
end subroutine
