subroutine xmele3(noma, modele, ligrel, nfiss, chelem,&
                  param, option)
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
    implicit none
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "jeveux.h"
!
    character(len=8) :: noma, modele
    character(len=*) :: param, option
    integer :: nfiss
    character(len=19) :: chelem
    character(len=19) :: ligrel
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - CREATION CHAM_ELEM)
!
! CREATION CHAM_ELEM RELATIFS AU CONTACT TYPE "MORTAR"
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  NFISS  : NOMBRE TOTAL DE FISSURES
! IN  LIGREL : NOM DU LIGREL DES MAILLES TARDIVES
! IN  CHELEM : NOM DU CHAM_ELEM A CREER
! IN  PARAM  : NOM DE PARAMETRE
!
!
!
!
    integer :: ifm, niv
    character(len=8) :: k8bid
    integer :: ibid, iad, i, ima, ifis, ier
    integer :: ino, itypma, nno
    integer :: ndim
    integer :: nbma, nmaenr
    character(len=8) :: nomfis, nomgd, typma, licmp3(3)
    integer :: jcesl, jcesd, jmofis, ncmp, icmp
    character(len=24) :: grp
    integer :: mminfi, jgrp, iret, ib1
    character(len=19) :: chelsi
    real(kind=8) :: valr
    character(len=8), pointer :: fiss(:) => null()
    integer, pointer :: typmail(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
!
    data licmp3    / 'X1', 'X2', 'X3'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<XFEM  > CREATION DU CHAM_ELEM PINDCOI '
    endif
!
! --- INITIALISATIONS CHAMPS SIMPLES DE TRAVAIL
!
    chelsi = '&&XMELE3.CES'
!
! --- RECUPERATION DES INFOS SUR LE MAILLAGE ET LE MODELE
!
    call jeveuo(modele//'.FISS', 'L', vk8=fiss)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call dismoi('DIM_GEOM', modele, 'MODELE', repi=ndim)
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
!
    ASSERT(param.eq.'PCOHES')
    nomgd = 'NEUT_R'
!
    ncmp = 3
!
! --- TEST EXISTENCE DU CHAM_ELEM OU NON
!
    call exisd('CHAM_ELEM', chelem, iret)
    if (iret .eq. 0) then
        call cescre('V', chelsi, 'ELNO', noma, nomgd,&
                    ncmp, licmp3, [-1], [-1], [-ncmp])
!
! --- RAZ VECTEUR DE DIMENSIONNEMENT
!
!      ENDIF
!
! --- ACCES AU CHAM_ELEM_S
!
        call jeveuo(chelsi//'.CESD', 'L', jcesd)
        call jeveuo(chelsi//'.CESL', 'E', jcesl)
        call jeveuo(chelsi//'.CESV', 'E', vr=cesv)
!
! --- ENRICHISSEMENT DU CHAM_ELEM_S POUR LA MULTIFISSURATION
!
        do 110 ifis = 1, nfiss
!
! --- ACCES FISSURE COURANTE
!
            nomfis = fiss(ifis)
            grp = nomfis(1:8)//'.MAILFISS.CONT'
            call jeexin(grp, iret)
            valr = 0.d0
!
! --- ON COPIE LES CHAMPS CORRESP. AUX ELEM. DE CONTACT
!
            if (iret .ne. 0) then
                call jeveuo(grp, 'L', jgrp)
                call jelira(grp, 'LONMAX', nmaenr, k8bid)
                do 120 i = 1, nmaenr
                    ima = zi(jgrp-1+i)
                    itypma = typmail(ima)
                    call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
                    call dismoi('NBNO_TYPMAIL', typma, 'TYPE_MAILLE', repi=nno)
!
! --- RECOPIE EFFECTIVE DES CHAMPS
!
                    do 150 ino = 1, nno
                        do 160 icmp = 1, ncmp
                            call cesexi('S', jcesd, jcesl, ima, ino,&
                                        1, icmp, iad)
                            zl(jcesl-1+abs(iad)) = .true.
                            cesv(abs(iad)) = valr
160                      continue
150                  continue
!
120              continue
            endif
110      end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
!
! on autorise un prolongement par zero
! sinon, il faudrait mettre NON a la place de OUI
        call cescel(chelsi, ligrel, option, param, 'NON',&
                    ib1, 'V', chelem, 'F', ibid)
!
! --- MENAGE
!
        call detrsd('CHAM_ELEM_S', chelsi)
    endif
!
    call jedema()
!
end subroutine
