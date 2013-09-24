subroutine xmele2(noma, modele, defico, ligrel, nfiss,&
                  chelem)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
!
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
    character(len=8) :: noma
    character(len=8) :: modele
    integer :: nfiss
    character(len=19) :: chelem
    character(len=19) :: ligrel
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - CREATION CHAM_ELEM)
!
! CREATION DU CHAM_ELEM PDONCO (DONNEES DU CONTACT)
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  NFISS  : NOMBRE TOTAL DE FISSURES
! IN  LIGREL : NOM DU LIGREL DES MAILLES TARDIVES
! IN  CHELEM : NOM DU CHAM_ELEM CREEE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
!
!
!
!
!
    integer :: nbcmp
    parameter     (nbcmp = 10)
    character(len=8) :: licmp(nbcmp)
!
    integer :: ifm, niv, iret
    integer :: ibid, iad, i, ima, ifis, izone
    integer :: nmaenr, nbma, jnbsp, ispt, icmp
    character(len=8) :: nomfis, k8bid
    integer :: jcesl, jcesv, jcesd, jmofis
    character(len=24) :: grp
    integer :: jgrp
    character(len=19) :: chelsi
    real(kind=8) :: coef(nbcmp)
    integer :: jmail
!
    data licmp    /'RHON','MU','RHOTK','INTEG','COECH',&
     &    'COSTCO','COSTFR','COPECO','COPEFR','RELA'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<XFEM  > CREATION DU CHAM_ELEM PDONCO'
    endif
!
! --- INITIALISATIONS
!
    chelsi = '&&XMELE2.CES'
!
! --- RECOPIE DU NOMBRE DE SOUS POINTS DE TOPOSE.HEA DANS LE CHAMP NBSP
!
    call celces(modele//'.TOPOSE.HEA', 'V', '&&XMELE2.HEAV')
    call jeveuo('&&XMELE2.HEAV      .CESD', 'L', jcesd)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8bid, ibid)
    call wkvect('&&XMELE2.NBSP', 'V V I', nbma, jnbsp)
    do 10 ima = 1, nbma
        zi(jnbsp-1+ima) = zi(jcesd-1+5+4*(ima-1)+2)
10  end do
!
!
!
! --- CREATION DU CHAM_ELEM_S
!
    call cescre('V', chelsi, 'ELEM', noma, 'XCONTAC',&
                nbcmp, licmp, [-1], zi(jnbsp), [-nbcmp])
!
! --- ACCES AU CHAM_ELEM_S
!
    call jeveuo(chelsi//'.CESD', 'L', jcesd)
    call jeveuo(chelsi//'.CESL', 'E', jcesl)
    call jeveuo(chelsi//'.CESV', 'E', jcesv)
!
! --- ACCES AUX FISSURES
!
    call jeveuo(modele//'.FISS', 'L', jmofis)
!
! --- RECUPERATION DES MAILLES DU MODELE
    call jeveuo(modele//'.MAILLE', 'L', jmail)
!
! --- ENRICHISSEMENT DU CHAM_ELEM POUR LA MULTIFISSURATION
!
    do 110 ifis = 1, nfiss
!
! --- ACCES FISSURE COURANTE
!
        nomfis = zk8(jmofis-1 + ifis)
!
! --- INFORMATIONS SUR LA FISSURE
!
        grp = nomfis(1:8)//'.MAILFISS.CONT'
!
! --- ZONE DE CONTACT IZONE CORRESPONDANTE
!
        izone = xxconi(defico,nomfis,'MAIT')
!
! --- CARACTERISTIQUES DU CONTACT POUR LA FISSURE EN COURS
!
        coef(1) = mminfr(defico,'COEF_AUGM_CONT',izone )
        coef(2) = mminfr(defico,'COEF_COULOMB' ,izone )
        coef(3) = mminfr(defico,'COEF_AUGM_FROT',izone )
        coef(4) = mminfi(defico,'INTEGRATION' ,izone )
        coef(6) = mminfr(defico,'ALGO_CONT' ,izone )
        coef(7) = mminfr(defico,'ALGO_FROT' ,izone )
        coef(8) = mminfr(defico,'COEF_PENA_CONT',izone )
        coef(9) = mminfr(defico,'COEF_PENA_FROT',izone )
        coef(10)= mminfr(defico,'RELATION' ,izone )
!
! --- ON COPIE LES CHAMPS CORRESP. AUX ELEM. HEAV, CTIP ET HECT
!
        call jeexin(grp, iret)
        if (iret .ne. 0) then
            call jeveuo(grp, 'L', jgrp)
            call jelira(grp, 'LONMAX', nmaenr)
            do 120 i = 1, nmaenr
                ima = zi(jgrp-1+i)
!
! --- RECUPERATION DU NUMÉRO DE SOUS POINT ISPT
!
                do 130 ispt = 1, zi(jnbsp-1+ima)
                    call cesexi('S', jcesd, jcesl, ima, 1,&
                                ispt, 1, iad)
                    if (iad .lt. 0) goto 140
130              continue
                ASSERT(.false.)
140              continue
!
! --- RECOPIE EFFECTIVE DES CHAMPS
!
                do 150 icmp = 1, nbcmp
                    call cesexi('S', jcesd, jcesl, ima, 1,&
                                ispt, icmp, iad)
                    zl(jcesl-1-iad) = .true.
                    zr(jcesv-1-iad) = coef(icmp)
150              continue
120          continue
        endif
!
110  end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
!
    call cescel(chelsi, ligrel, 'XCVBCA', 'PDONCO', 'NON',&
                ibid, 'V', chelem, 'F', ibid)
!
! --- MENAGE
!
    call detrsd('CHAM_ELEM_S', chelsi)
    call detrsd('CHAM_ELEM_S', '&&XMELE2.HEAV')
    call jedetr('&&XMELE2.NBSP')
!
    call jedema()
!
end subroutine
