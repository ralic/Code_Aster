subroutine xfisno(noma, modelx)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
!
    character(len=8) :: noma, modelx
!
!----------------------------------------------------------------------
!  BUT: CREATION D'UN CHAMPS ELNO QUI ASSOCIE POUR CHAQUE NOEUD LE
!       NUMÉRO DE FISSURE LOCALE AU DDL HEAVISIDE
!
!----------------------------------------------------------------------
!
!     ARGUMENTS/
!  NOMA       IN       K8 : MAILLAGE
!  MODELX     IN/OUT   K8 : MODELE XFEM
!
!
!
!
!
    integer :: jlcnx, jcesfd, jcesfl, jcesd, jcesl
    integer :: jcesd2, jcesl2
    integer :: nbma, ima, nbno, ino, nheav, iheav, nfiss, ifiss
    integer :: ibid, iad, nncp
    integer, pointer :: p_mail_affe(:) => null()
    character(len=19) :: fissno, ces, cesf, ligrel, ces2, heavno
    aster_logical :: lcont
    integer, pointer :: xfem_cont(:) => null()
    integer, pointer :: cesfv(:) => null()
    integer, pointer :: cesv2(:) => null()
    integer, pointer :: cesv(:) => null()
    integer, pointer :: nbsp2(:) => null()
    integer, pointer :: nbsp(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ligrel = modelx(1:8)//'.MODELE'
    fissno = modelx(1:8)//'.FISSNO'
    ces = '&&XFISNO.FISSNO'
    cesf = '&&XFISNO.STNO'
!
! --- LE CONTACT EST-IL DÉCLARÉ
!
    call jeveuo(modelx(1:8)//'.XFEM_CONT', 'L', vi=xfem_cont)
    ASSERT(xfem_cont(1).le.1 .or. xfem_cont(1).eq.2 .or. xfem_cont(1).eq.3)
    lcont = (xfem_cont(1).eq.1 .or. xfem_cont(1).eq.2 .or. xfem_cont(1).eq.3)
    if (lcont) then
        heavno = modelx(1:8)//'.HEAVNO'
        ces2 = '&&XFISNO.HEAVNO'
    endif
!
! --- TRANSFO CHAM_ELEM -> CHAM_ELEM_S DE STANO
!
    call celces(modelx(1:8)//'.STNO', 'V', cesf)
!
    call jeveuo(cesf//'.CESD', 'L', jcesfd)
    call jeveuo(cesf//'.CESV', 'L', vi=cesfv)
    call jeveuo(cesf//'.CESL', 'L', jcesfl)
!
! --- RECUPERATION DE LA LISTE DES MAILLES AFFECTEES PAR DES EF
!
    call jeveuo(modelx//'.MAILLE', 'L', vi=p_mail_affe)

!
! --- RECUPERATION DU NOMBRE DE FISSURES VUES
!
    call jeveuo('&&XTYELE.NBSP', 'L', vi=nbsp)
!
!
! --- RECUPERATION DU NOMBRE DE FONCTIONS HEAVISIDES
!
    call jeveuo('&&XTYELE.NBSP2', 'L', vi=nbsp2)
!
! --- CREATION DE LA SD ELNO FISSNO
!
    call cescre('V', ces, 'ELNO', noma, 'NEUT_I',&
                1, 'X1', [ibid], nbsp2, [-1])
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESV', 'E', vi=cesv)
    call jeveuo(ces//'.CESL', 'E', jcesl)
!
! --- SI CONTACT, CREATION DE LA SD ELNO HEAVNO
!
    if (lcont) then
        call cescre('V', ces2, 'ELNO', noma, 'NEUT_I',&
                    1, 'X1', [ibid], nbsp, [-1])
!
        call jeveuo(ces2//'.CESD', 'L', jcesd2)
        call jeveuo(ces2//'.CESV', 'E', vi=cesv2)
        call jeveuo(ces2//'.CESL', 'E', jcesl2)
    endif
!
! --- INFOS SUR LE MAILLAGE
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jlcnx)
!
    do ima = 1, nbma
!       la maille est-elle affectee par un ef ?
        if (p_mail_affe(ima) .eq. 0) cycle
        nfiss = nbsp(ima)
        nheav = nbsp2(ima)
        if (nfiss .ge. 2) then
            nbno = zi(jlcnx+ima)-zi(jlcnx-1+ima)
            do ino = 1, nbno
!
! --- PREMIERE PASSE, ON REMPLIT AVEC LES HEAVISIDES ACTIFS
!
                do ifiss = 1, nfiss
                    call cesexi('S', jcesfd, jcesfl, ima, ino,&
                                ifiss, 1, iad)
                    ASSERT(iad.gt.0)
                    if (cesfv(iad) .eq. 1) then
                        do iheav = 1, nheav
                            call cesexi('S', jcesd, jcesl, ima, ino,&
                                        iheav, 1, iad)
                            if (iad .lt. 0) then
                                zl(jcesl-1-iad) = .true.
                                cesv(1-1-iad) = ifiss
                                if (lcont) then
                                    call cesexi('S', jcesd2, jcesl2, ima, ino,&
                                                ifiss, 1, iad)
                                    ASSERT(iad.lt.0)
                                    zl(jcesl2-1-iad) = .true.
                                    cesv2(1-1-iad) = iheav
                                endif
                                goto 30
                            endif
                        end do
                    endif
 30                 continue
                end do
!
! --- DEUXIEME PASSE, ON REMPLIT AVEC LES HEAVISIDES INACTIFS
!
                do ifiss = 1, nfiss
                    call cesexi('S', jcesfd, jcesfl, ima, ino,&
                                ifiss, 1, iad)
                    ASSERT(iad.gt.0)
                    if (cesfv(iad) .eq. 0) then
                        do iheav = 1, nheav
                            call cesexi('S', jcesd, jcesl, ima, ino,&
                                        iheav, 1, iad)
                            if (iad .lt. 0) then
                                zl(jcesl-1-iad) = .true.
                                cesv(1-1-iad) = ifiss
                                if (lcont) then
                                    call cesexi('S', jcesd2, jcesl2, ima, ino,&
                                                ifiss, 1, iad)
                                    ASSERT(iad.lt.0)
                                    zl(jcesl2-1-iad) = .true.
                                    cesv2(1-1-iad) = iheav
                                endif
                                goto 50
                            endif
                        end do
                    endif
 50                 continue
                end do
!
! --- FIN DES DEUX PASSES, FISSNO EST DEFINI ENTIEREMENT POUR LE NOEUD
! ---                      HEAVNO N'EST PAS COMPLET
!
            end do
        endif
    end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
!
    call cescel(ces, ligrel, 'INI_XFEM_ELNO', 'PFISNO', 'OUI',&
                nncp, 'G', fissno, 'F', ibid)
    call detrsd('CHAM_ELEM_S', ces)
    if (lcont) then
        call cescel(ces2, ligrel, 'FULL_MECA', 'PHEAVNO', 'NAN',&
                    nncp, 'G', heavno, 'F', ibid)
        call detrsd('CHAM_ELEM_S', ces2)
    endif
    call detrsd('CHAM_ELEM_S', cesf)
    call jedema()
end subroutine
