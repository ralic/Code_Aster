subroutine xmele1(noma, modele, defico, ligrel, nfiss,&
                  chelem, param, option)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfr.h"
#include "asterfort/wkvect.h"
#include "asterfort/xmelin.h"
#include "asterfort/xxconi.h"
!
    character(len=8) :: noma, modele
    character(len=*) :: param, option
    integer :: nfiss
    character(len=19) :: chelem
    character(len=19) :: ligrel
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - CREATION CHAM_ELEM)
!
! CREATION CHAM_ELEM RELATIFS AU CONTACT
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
    integer :: ibid, iad, ispt, i, ima, ifis, izone
    integer :: ifima, jcesd1, jcesl1, jcesv1, jxc
    integer :: ndim, nface, nfisc, nnint, npg, typint, nfisc2
    integer :: nbma, nmaenr, jcesd2, jcesl2, jcesv2
    character(len=8) :: nomfis, nomgd, elc, nomfi2, licmp3(3)
    integer :: jcesl, jcesv, jcesd, jmofis, ncmp, icmp
    character(len=24) :: grp
    integer :: jgrp, iret, jnbsp, ifiss
    logical :: vall, isint
    character(len=19) :: chelsi, cmafis, faclon, chnbsp
    real(kind=8) :: valr
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
    chelsi = '&&XMELE1.CES'
    faclon = '&&XMELE1.FACLO'
    chnbsp = '&&XMELE1.NBSP'
    cmafis = '&&XMELE1.XMAFIS'
!
! --- RECUPERATION DES INFOS SUR LE MAILLAGE ET LE MODELE
!
    call jeveuo(modele//'.FISS', 'L', jmofis)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call dismoi('DIM_GEOM', modele, 'MODELE', repi=ndim)
    call jeveuo(modele//'.XFEM_CONT', 'L', jxc)
!
! --- ELEMENT DE REFERENCE ASSOCIE A UNE FACETTE DE CONTACT
!
    if (ndim .eq. 3) then
        elc='TR3'
    else if (ndim.eq.2) then
        if (zi(jxc) .le. 2) then
            elc='SE2'
        else
            elc='SE3'
        endif
    endif
!
! --- INFOS SUR LE TYPE DE LA VALEUR A INITIALISER
!
    isint = (param.eq.'PINDCOI'.or. param.eq.'PMEMCON'.or. param.eq.'PGLISS')
    if (isint) then
        nomgd = 'NEUT_I'
    else
        nomgd = 'NEUT_R'
    endif
!
    if (param .eq. 'PCOHES') then
        ncmp = 3
    else
        ncmp = 1
    endif
!
! --- CREATION DES OBJETS DE TRAVAIL
!
    call celces(modele//'.TOPOFAC.LO', 'V', faclon)
    call jeveuo(faclon(1:19)//'.CESD', 'L', jcesd2)
    call jeveuo(faclon(1:19)//'.CESL', 'L', jcesl2)
    call jeveuo(faclon(1:19)//'.CESV', 'L', jcesv2)
    call celces(modele//'.XMAFIS', 'V', cmafis)
    call jeveuo(cmafis(1:19)//'.CESD', 'L', jcesd1)
    call jeveuo(cmafis(1:19)//'.CESL', 'L', jcesl1)
    call jeveuo(cmafis(1:19)//'.CESV', 'L', jcesv1)
    call wkvect(chnbsp, 'V V I', nbma, jnbsp)
!
! --- TEST EXISTENCE DU CHAM_ELEM OU NON
!
    call exisd('CHAM_ELEM', chelem, iret)
    if (iret .ne. 0) then
        call celces(chelem, 'V', chelsi)
    else
!
! ---  BOUCLE SUR LES FISSURES
!
        do ifiss = 1, nfiss
!
! --- RECUPERATION NOMBRE DE POINTS DE GAUSS PAR FACETTE
!
            nomfis = zk8(jmofis-1 + ifiss)
            izone = xxconi(defico,nomfis,'MAIT')
            typint = mminfi(defico,'INTEGRATION',izone )
            call xmelin(elc, typint, nnint)
!
! --- RECUP LISTE DES MAILLES DE CONTACT POUR LA FISSURE
!
            grp = nomfis(1:8)//'.MAILFISS.CONT'
            call jeexin(grp, iret)
            nmaenr = 0
            if (iret .ne. 0) then
                call jeveuo(grp, 'L', jgrp)
                call jelira(grp, 'LONMAX', nmaenr)
            endif
!
! --- BOUCLE SUR LES MAILLES DE CONTACT DE LA FISSURE
!
            do i = 1, nmaenr
                ima = zi(jgrp-1+i)
                ASSERT(ima.le.nbma)
! --- NOMBRE DE FISSURES VUE PAR LA MAILLE
                nfisc = zi(jcesd2-1+5+4*(ima-1)+2)
                nfisc2 = zi(jcesd1-1+5+4*(ima-1)+2)
                ASSERT(nfisc.eq.nfisc2)
                nface = 0
! --- BOUCLE SUR CES FISSURES
                do ifima = 1, nfisc
                    call cesexi('S', jcesd1, jcesl1, ima, 1,&
                                ifima, 1, iad)
                    nomfi2 = zk8(jcesv1-1+iad)
!
! --- S IL S AGIT DE LA FISSURE COURANTE
! --- ON RECUPERE LE NOMBRE DE FACETTES ET ON INCREMENTE
! --- LE NOMBRE DE SOUS-POINTS
!
                    if (nomfis .eq. nomfi2) then
                        ASSERT(nface.eq.0)
                        call cesexi('S', jcesd2, jcesl2, ima, 1,&
                                    ifima, 2, iad)
                        nface = zi(jcesv2-1+iad)
                        ASSERT(nface.le.6)
                        zi(jnbsp-1+ima) = zi(jnbsp-1+ima)+nface*nnint
                    endif
                end do
            end do
        end do
!
        if (param .eq. 'PCOHES') then
            call cescre('V', chelsi, 'ELEM', noma, nomgd,&
                        1, licmp3, [-1], zi( jnbsp), [-ncmp])
        else
            call cescre('V', chelsi, 'ELEM', noma, nomgd,&
                        1, 'X1', [-1], zi( jnbsp), [-ncmp])
        endif
!
! --- RAZ VECTEUR DE DIMENSIONNEMENT
!
    endif
    call jedetr(chnbsp)
    call wkvect(chnbsp, 'V V I', nbma, jnbsp)
!
! --- ACCES AU CHAM_ELEM_S
!
    call jeveuo(chelsi//'.CESD', 'L', jcesd)
    call jeveuo(chelsi//'.CESL', 'E', jcesl)
    call jeveuo(chelsi//'.CESV', 'E', jcesv)
!
! --- ENRICHISSEMENT DU CHAM_ELEM_S POUR LA MULTIFISSURATION
!
    do ifis = 1, nfiss
!
! --- ACCES FISSURE COURANTE
!
        nomfis = zk8(jmofis-1 + ifis)
        grp = nomfis(1:8)//'.MAILFISS.CONT'
        call jeexin(grp, iret)
        if (iret .ne. 0) call jeveuo(grp, 'L', jgrp)
!
! --- ZONE DE CONTACT IZONE CORRESPONDANTE
!
        izone = xxconi(defico,nomfis,'MAIT')
        typint = mminfi(defico,'INTEGRATION',izone )
        call xmelin(elc, typint, nnint)
!
! --- CONTACT INIT
!
        if (isint) then
            if (param .eq. 'PINDCOI' .or. param .eq. 'PMEMCON') then
                vall = mminfi(defico,'CONTACT_INIT',izone ).eq.1
            else if (param.eq.'PGLISS') then
                vall = mminfl(defico,'GLISSIERE_ZONE',izone )
            else
                ASSERT(.false.)
            endif
        else
            if (param .eq. 'PSEUIL') then
                valr = mminfr(defico,'SEUIL_INIT',izone )
            else if (param(1:5).eq.'PCOHE') then
                valr = 0.d0
            else
                ASSERT(.false.)
            endif
            vall = valr.ne.0.d0
        endif
!
        if (vall) then
!
! --- ON COPIE LES CHAMPS CORRESP. AUX ELEM. DE CONTACT
!
            call jeexin(grp, iret)
            if (iret .ne. 0) then
                call jeveuo(grp, 'L', jgrp)
                call jelira(grp, 'LONMAX', nmaenr)
                do i = 1, nmaenr
                    ima = zi(jgrp-1+i)
!
! --- INDICE LOCAL DE LA FISSURE COURANTE
! --- ET DECALAGE CORRESPONDANT
!
                    nfisc = zi(jcesd1-1+5+4*(ima-1)+2)
                    do ifima = 1, nfisc
                        call cesexi('S', jcesd1, jcesl1, ima, 1,&
                                    ifima, 1, iad)
                        nomfi2 = zk8(jcesv1-1+iad)
                        if (nomfis .eq. nomfi2) then
                            call cesexi('S', jcesd2, jcesl2, ima, 1,&
                                        ifima, 2, iad)
                            nface = zi(jcesv2-1+iad)
                            npg = nface*nnint
                        endif
                    end do
!
! --- RECOPIE EFFECTIVE DES CHAMPS
!
                    do ispt = 1, npg
                        do icmp = 1, ncmp
                            call cesexi('S', jcesd, jcesl, ima, 1,&
                                        zi(jnbsp-1+ima)+ispt, icmp, iad)
                            zl(jcesl-1+abs(iad)) = .true.
                            if (isint) zi(jcesv-1+abs(iad)) = 1
                            if (.not.isint) zr(jcesv-1+abs(iad)) = valr
                        end do
                    end do
!
! --- INCREMENTATION REPERAGE POUR LES FISSURES SUIVANTES
!
                    zi(jnbsp-1+ima) = zi(jnbsp-1+ima) + npg
!
                end do
            endif
        endif
    end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
!
    call cescel(chelsi, ligrel, option, param, 'OUI',&
                ibid, 'V', chelem, 'F', ibid)
!
! --- MENAGE
!
    call detrsd('CHAM_ELEM_S', chelsi)
    call detrsd('CHAM_ELEM_S', cmafis)
    call detrsd('CHAM_ELEM_S', faclon)
    call jedetr(chnbsp)
!
    call jedema()
!
end subroutine
