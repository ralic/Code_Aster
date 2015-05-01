subroutine calvci(nomci, nume_ddlz, nbchci, lchci, inst,&
                  base)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsinch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: nomci, lchci(*), nume_ddlz
    character(len=1) :: base
    real(kind=8) :: inst
    integer :: nbchci
! ----------------------------------------------------------------------
! BUT  :  CALCUL DU CHAM_NO CONTENANT UN VECTEUR LE CINEMATIQUE
! ---     ASSOCIE A UNE LISTE DE CHAR_CINE_* A UN INSTANT INST
!         CHAR_CINE ET AYANT COMME PROF_CHNO CELUI DE NOMNU
!                              ---
!                              ! 0. SI I DDLS NON IMPOSE DANS
!         NOMCI(1:19).VALE(I) =!       LA LISTE DES CHAR_CINE
!                              ! U0(NI,INST) SINON
!                              ---
!             OU NI EST LE NUMERO DANS LE MAILLAGE DU NOEUD
!                   SUPPORTANT LE DDL NUMERO I DANS LA NUMEROTATION
!          U0(NI,INST)= VALEUR DU CHARGEMENT ASSOCIE A LA
!                       DERNIERE CHAR_CINE IMPOSANT I
! ----------------------------------------------------------------------
! IN/JXVAR  K*19 NOMCI  : NOM DU CHAM_NO CREE A PARTIR DE LA LISTE DE
!                   CHAR_CINE ET AYANT COMME PROF_CHNO CELUI DE NOMNU
! IN  K*14 NUME_DDL  : NOM DE LA NUMEROTATION SUPPORTANT LE CHAM_NO
! IN  I    NBCHCI : NOMBRE DE CHAR_CINE DE LA LISTE LCHCI
! IN  K*24 LCHCI  : LISTE DES NOMS DES CHARGES CINEMATIQUES ENTRANT
!                   DANS LE CALCUL DU CHAM_NO NOMCI
! IN  R*8  INST   : INSTANT
! IN  K*1  BASE   : BASE SUR LAQUELLE ON CREE LE CHAM_NO
!-----------------------------------------------------------------------
!     FONCTIONS JEVEUX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    integer :: iddes, nec, ivvale,  jprno,  ichcin
    integer :: jafcv, nb_affe_cine, i_affe_cine, i_node, i_cmp, i_eq, ier
    integer :: neq, numgd,  jdlci, i_nueq
    integer :: jcn1l, i_cmp_gran, i_cmp_gran1, jnocmp
    integer :: nbcmp1,  i_ligr_mesh, vali(1)
    character(len=1) :: typval
    character(len=4) :: phen
    aster_logical :: fonc
    real(kind=8) :: valp(4), res, valr(1)
    character(len=8) :: mesh, gd, nomf, evoim, cmp_name, nomch
    character(len=14) :: nume_ddl
    character(len=16) :: nomp(4)
    character(len=19) :: vcine, charci, cnoimp, cnsimp, prof_chno
    character(len=24) :: vvale, valk(4)
    integer, pointer :: afci(:) => null()
    integer, pointer :: cnsd(:) => null()
    character(len=8), pointer :: afck(:) => null()
    integer, pointer :: p_nueq(:) => null()
    real(kind=8), pointer :: cnsv(:) => null()
    character(len=8), pointer :: cnsc(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: deeq(:) => null()
    character(len=8), pointer :: typegd(:) => null()
!----------------------------------------------------------------------
!                DEBUT DES INSTRUCTIONS
!----------------------------------------------------------------------
    call jemarq()
    if (nbchci .eq. 0) goto 999
    vcine = nomci
    nume_ddl = nume_ddlz
    vvale = vcine//'.VALE'
    valr(1)=inst
    cnoimp='&&CALVCI.CNOIMP'
    cnsimp='&&CALVCI.CNSIMP'
!
! - Get informations about NUME_DDL
!
    call dismoi('NOM_GD'    , nume_ddl, 'NUME_DDL', repk=gd)
    call dismoi('NOM_MAILLA', nume_ddl, 'NUME_DDL', repk=mesh)
    call dismoi('PROF_CHNO' , nume_ddl, 'NUME_DDL', repk=prof_chno)
!
! - Get informations about GRANDEUR
!
    call jenonu(jexnom('&CATA.GD.NOMGD', gd), numgd)
    call jeveuo('&CATA.GD.TYPEGD', 'L', vk8=typegd)
    typval = typegd(numgd)(1:1)
    call jeveuo(jexnum('&CATA.GD.DESCRIGD', numgd), 'L', iddes)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', jnocmp)
    nec = zi(iddes+2 )
!
! - Create CHAM_NO
!
    call detrsd('CHAMP_GD', vcine)
    call vtcreb(vcine, base, typval,&
                nume_ddlz = nume_ddl,&
                nb_equa_outz = neq)
!
! - Create DLCI object (see nmcvci.F90)
!
    call jedetr(vcine//'.DLCI')
    call wkvect(vcine//'.DLCI', 'V V I', neq, jdlci)
!
    call jeveuo(vvale, 'E', ivvale)
    call jeveuo(prof_chno//'.NUEQ', 'L', vi=p_nueq)
    call jeveuo(prof_chno//'.DEEQ', 'L', vi=deeq)
    call jenonu(jexnom(prof_chno//'.LILI', '&MAILLA'), i_ligr_mesh)
    call jeveuo(jexnum(prof_chno//'.PRNO', i_ligr_mesh), 'L', jprno)
    call jeveuo(mesh//'.COORDO    .VALE', 'L', vr=vale)
!
! - Loop on kinematic loads
!
    do ichcin = 1, nbchci
        charci = lchci(ichcin)
        call jeveuo(charci//'.AFCK', 'L', vk8=afck)
        phen=afck(1)(1:4)
        fonc=afck(1)(5:7).eq.'_FT'
        evoim=afck(3)
        call jeveuo(charci//'.AFCI', 'L', vi = afci)
        if (evoim .eq. ' ') then
            call jeveuo(charci//'.AFCV', 'L', jafcv)
        endif
!
!
!       -- CAS DE EVOL_IMPO : ON PREPARE ...
!       ---------------------------------------
        if (evoim .ne. ' ') then
!         -- IL FAUT INTERPOLER EVOIM A L'INSTANT: INST :
            if (gd .eq. 'DEPL_R') then
                nomch='DEPL'
            else if (gd.eq.'TEMP_R') then
                nomch='TEMP'
            else
                ASSERT(.false.)
            endif
            ASSERT(fonc)
            call rsinch(evoim, nomch, 'INST', inst, cnoimp,&
                        'EXCLU', 'EXCLU', 2, 'V', ier)
            call cnocns(cnoimp, 'V', cnsimp)
            call detrsd('CHAMP', cnoimp)
            call jeveuo(cnsimp//'.CNSD', 'L', vi=cnsd)
            call jeveuo(cnsimp//'.CNSC', 'L', vk8=cnsc)
            call jelira(cnsimp//'.CNSC', 'LONMAX', nbcmp1)
            ASSERT(nbcmp1.eq.cnsd(2))
            call jeveuo(cnsimp//'.CNSV', 'L', vr=cnsv)
            call jeveuo(cnsimp//'.CNSL', 'L', jcn1l)
            valk(1)=evoim
        endif
!
!
!       -- AFFECTATION DES VALEURS IMPOSEES
!       ---------------------------------------
        nb_affe_cine = afci(1)
!
!
!
!       -- CAS DES VALEURS REELLES :
!       ---------------------------------
        if (typval .eq. 'R') then
            do i_affe_cine = 1, nb_affe_cine
! ------------- i_node: index of node
! ------------- i_cmp : index of component for LOCAL grandeur for current node
                i_node = afci(3*(i_affe_cine-1)+2)
                i_cmp  = afci(3*(i_affe_cine-1)+3)
                i_nueq = zi(jprno+(nec+2)*(i_node-1))
                i_eq   = p_nueq(i_nueq+i_cmp-1)
!
!           -- CAS EVOL_IMPO (CNSIMP):
!           ----------------------------------
                if (evoim .ne. ' ') then
                    i_cmp_gran = deeq(2*(i_eq-1)+2)
                    cmp_name   = zk8(jnocmp-1+i_cmp_gran)
                    vali(1)    = i_node
                    valk(2)    = cmp_name
                    i_cmp_gran1=indik8(cnsc,cmp_name,1,nbcmp1)
                    ASSERT(i_cmp_gran1.gt.0)
                    if (.not.zl(jcn1l-1+(i_node-1)*nbcmp1+i_cmp_gran1)) then
                        call utmess('F', 'CALCULEL_2', nk=2, valk=valk, si=vali(1),&
                                    sr=valr(1))
                    endif
                    zr(ivvale-1+i_eq) = cnsv((i_node-1)*nbcmp1+i_cmp_gran1)
!
!
!           -- CAS "NORMAL" (OBJET .AFCV) :
!           ----------------------------------
                else if (.not.fonc) then
                    zr(ivvale-1+i_eq) = zr(jafcv-1+i_affe_cine)
!
!
!           -- CAS FONCTION :
!           -----------------
                else if (fonc) then
                    nomf = zk8(jafcv-1+i_affe_cine)
                    nomp(1)='INST'
                    nomp(2)='X'
                    nomp(3)='Y'
                    nomp(4)='Z'
                    valp(1)=inst
                    valp(2)=vale(1+3*(i_node-1)+0)
                    valp(3)=vale(1+3*(i_node-1)+1)
                    valp(4)=vale(1+3*(i_node-1)+2)
                    call fointe('F ', nomf, 4, nomp, valp,&
                                res, ier)
                    zr(ivvale-1+i_eq) = res
                else
                    call utmess('F', 'CALCULEL_37')
                endif
!
                zi(jdlci-1+i_eq) = 1
            end do
!
!
!
!       -- CAS DES VALEURS COMPLEXES :
!       ---------------------------------
        else if (typval.eq.'C') then
            ASSERT(phen.eq.'CIAC')
            ASSERT(.not.fonc)
            do i_affe_cine = 1, nb_affe_cine
! ------------- i_node: index of node
! ------------- i_cmp : index of component for LOCAL grandeur for current node
                i_node = afci(3*(i_affe_cine-1)+2)
                i_cmp  = afci(3*(i_affe_cine-1)+3)
                i_nueq = zi(jprno+(nec+2)*(i_node-1))
                i_eq   = p_nueq(i_nueq+i_cmp-1)

                zc(ivvale-1+i_eq) = zc(jafcv-1+i_affe_cine)
                zi(jdlci-1+i_eq) = 1
            end do
!
!
        else
            ASSERT(.false.)
        endif
    end do
!
999 continue
!
    call detrsd('CHAMP', cnsimp)
    call jedema()
end subroutine
