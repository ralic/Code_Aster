subroutine xinils(noma, maiaux, grille, ndim, meth,&
                  nfonf, nfong, geofis, a, b,&
                  r, noeud, cote, vect1, vect2,&
                  cnslt, cnsln)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xajuls.h"
#include "asterfort/xcatls.h"
#include "asterfort/xls2d.h"
#include "asterfort/xls3d.h"
!
    character(len=8) :: noma, meth, nfonf, nfong, cote
    character(len=8) :: maiaux
    character(len=16) :: geofis
    character(len=19) :: cnslt, cnsln
    real(kind=8) :: a, b, r, noeud(3), vect1(3), vect2(3)
    aster_logical :: grille
!
! ----------------------------------------------------------------------
!                      CALCUL INITIAL DES LEVEL-SETS
!
! ENTREE :
!  NOMA   :  OBJET MAILLAGE
!  FISS   :  NOM DE LA FISSURE A CREER
!  MAIAUX :  OBJET MAILLAGE SUR LEQUEL LA GRILLE AUXILIAIRE EST DEFINIE.
!            ' ' SI AUCUNE GRILLE EST DEMANDE.
!  GRILLE :  .TRUE. SI UN GRILLE A ETE DEMANDEE. LE MAILLAGE MAIAUX SERA
!                   UTILISE POUR DEFINIR LA GRILLE.
!            .FALSE. SI UN GRILLE N'A PAS ETE DEMANDE.
!  METH   :  METHODE DE CALUL DES LEVEL-SETS
!  NFONF  :  NOM DE LA FONCTION LEVEL SET TANGENTE
!  NFONG  :  NOM DE LA FONCTION LEVEL SET NORMALE
!  GEOFIS :  GEOMETRIE DE LA FISSURE
!  A,B,R,NOEUD,COTE,VECT1,VECT2 :
!            QUANTITES DEFINISSANT LA GEO DE LA FISS
! SORTIE :
!      CNSLN  :  LEVEL-SET NORMALE  (PLAN DE LA FISSURE)
!      CNSLT  :  LEVEL-SET TANGENTE (TRACE DE LA FISSURE)
!
!     ------------------------------------------------------------------
!
    integer :: jconx1, jconx2, jdlima, jdlise
    integer :: nbma, nbsef
    integer :: jcnl, jctl, nbmagr, jcong1, jcong2, nbnoc
    integer :: jcoorc
    real(kind=8) :: xln, xlt
    integer :: ndim, dimno
    integer :: ibid, clsm, me4
    integer :: nbno, nbnogr, ino, jcoor, jcoorg, nbmaf
    integer :: jltsv, jltsl, jlnsv, jlnsl
    real(kind=8) :: valpu(3)
    character(len=8) :: fiss, nompu(3), nchamn, nchamt
    character(len=16) :: k16bid, typdis
    character(len=19) :: chslsn, chslst
    character(len=24) :: lisma, lisse
    aster_logical :: callst
    integer :: ifm, niv
    integer, pointer :: cnd(:) => null()
    integer, pointer :: ctd(:) => null()
    real(kind=8), pointer :: cnv(:) => null()
    real(kind=8), pointer :: ctv(:) => null()
!
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
    if (niv .ge. 3) write(ifm,*)'CALCUL DES LEVEL-SETS'
!
    call getres(fiss, k16bid, k16bid)
!
    nompu(1)='X'
    nompu(2)='Y'
    nompu(3)='Z'
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
    call jeveuo(cnslt//'.CNSV', 'E', jltsv)
    call jeveuo(cnslt//'.CNSL', 'E', jltsl)
    call jeveuo(cnsln//'.CNSV', 'E', jlnsv)
    call jeveuo(cnsln//'.CNSL', 'E', jlnsl)
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!     ELABORATE THE CASE "GRILLE AUXILIAIRE"
    if (grille) then
        call dismoi('NB_NO_MAILLA', maiaux, 'MAILLAGE', repi=nbnogr)
        call dismoi('NB_MA_MAILLA', maiaux, 'MAILLAGE', repi=nbmagr)
        call jeveuo(maiaux//'.COORDO    .VALE', 'L', jcoorg)
        call jeveuo(maiaux//'.CONNEX', 'L', jcong1)
        call jeveuo(jexatr(maiaux//'.CONNEX', 'LONCUM'), 'L', jcong2)
    endif
!
    call dismoi('TYPE_DISCONTINUITE', fiss, 'FISS_XFEM', repk=typdis)
    if (typdis .eq. 'INTERFACE') callst = .false.
    if (typdis .eq. 'FISSURE') callst = .true.
!
    if (meth .eq. 'FONCTION') then
!
!-----------------------------------------------------------------------
!       DANS LE CAS OU ON DONNE FONC_LT ET FONC_LN
!-----------------------------------------------------------------------
!
        if (grille) then
            nbnoc = nbnogr
            jcoorc = jcoorg
        else
            nbnoc = nbno
            jcoorc = jcoor
        endif
!
        do ino = 1, nbnoc
            do dimno = 1, ndim
                valpu(dimno)=zr(jcoorc-1+3*(ino-1)+dimno)
            end do
            call fointe('F ', nfong, ndim, nompu, valpu,&
                        xln, ibid)
            if (callst) then
                call fointe('F ', nfonf, ndim, nompu, valpu,&
                            xlt, ibid)
            else
                xlt = -1.d0
            endif
            zr(jlnsv-1+(ino-1)+1)=xln
            zr(jltsv-1+(ino-1)+1)=xlt
            zl(jltsl-1+(ino-1)+1)=.true.
            zl(jlnsl-1+(ino-1)+1)=.true.
        end do
!
    else if (meth.eq.'GROUP_MA') then
!
!-----------------------------------------------------------------------
!       DANS LE CAS OU ON DONNE GROUP_MA_FISS ET GROUP_MA_FOND
!-----------------------------------------------------------------------
!
        lisma = '&&XINILS.LISTE_MA_FISSUR'
        call reliem(' ', noma, 'NU_MAILLE', 'DEFI_FISS', 1,&
                    1, 'GROUP_MA_FISS', 'GROUP_MA', lisma, nbmaf)
        call jeveuo(lisma, 'L', jdlima)
!
        if (callst) then
            lisse = '&&XINILS.LISTE_MA_FONFIS'
            call reliem(' ', noma, 'NU_MAILLE', 'DEFI_FISS', 1,&
                        1, 'GROUP_MA_FOND', 'GROUP_MA', lisse, nbsef)
            call jeveuo(lisse, 'L', jdlise)
        endif
!
        if (ndim .eq. 3) then
            if (grille) then
                call xls3d(callst, grille, jltsv, jltsl, jlnsv,&
                           jlnsl, nbnogr, jcoor, jcoorg, nbmaf,&
                           jdlima, nbsef, jdlise, jconx1, jconx2,&
                           noma)
            else
                call xls3d(callst, grille, jltsv, jltsl, jlnsv,&
                           jlnsl, nbno, jcoor, jcoorg, nbmaf,&
                           jdlima, nbsef, jdlise, jconx1, jconx2,&
                           noma)
            endif
        else
            if (grille) then
                call xls2d(callst, grille, jltsv, jltsl, jlnsv,&
                           jlnsl, nbnogr, jcoor, jcoorg, nbmaf,&
                           jdlima, nbsef, jdlise, jconx1, jconx2)
            else
                call xls2d(callst, grille, jltsv, jltsl, jlnsv,&
                           jlnsl, nbno, jcoor, jcoorg, nbmaf,&
                           jdlima, nbsef, jdlise, jconx1, jconx2)
            endif
        endif
!
    else if (meth.eq.'GEOMETRI') then
!
!-----------------------------------------------------------------------
!       DANS LE CAS OU ON DONNE LA GEOMETRIE DE LA FISSURE
!-----------------------------------------------------------------------
!
        if (grille) then
            call xcatls(ndim, geofis, callst, jltsv, jltsl,&
                        jlnsv, jlnsl, maiaux, vect1, vect2,&
                        noeud, a, b, r, cote)
        else
            call xcatls(ndim, geofis, callst, jltsv, jltsl,&
                        jlnsv, jlnsl, noma, vect1, vect2,&
                        noeud, a, b, r, cote)
        endif
!
    else if (meth.eq.'CHAMP') then
!
!-----------------------------------------------------------------------
!       DANS LE CAS OU ON DONNE UN CHAMP DE LEVEL SET
!-----------------------------------------------------------------------
!
        call getvid('DEFI_FISS', 'CHAM_NO_LSN', iocc=1, scal=nchamn, nbret=me4)
        call getvid('DEFI_FISS', 'CHAM_NO_LST', iocc=1, scal=nchamt, nbret=ibid)
!
        chslsn='&&XINILS.CHAM_S_LSN'
        chslst='&&XINILS.CHAM_S_LST'
        call cnocns(nchamn, 'V', chslsn)
        if (callst) call cnocns(nchamt, 'V', chslst)
!
!       ON VERIFIE LE NOMBRE DE COMPOSANTES = 1  (LSN OU LST)
        call jeveuo(chslsn//'.CNSD', 'L', vi=cnd)
        ASSERT(cnd(2).eq.1)
        if (callst) call jeveuo(chslst//'.CNSD', 'L', vi=ctd)
        if (callst) then
            ASSERT(ctd(2).eq.1)
        endif
!
        call jeveuo(chslsn//'.CNSV', 'L', vr=cnv)
        call jeveuo(chslsn//'.CNSL', 'L', jcnl)
        if (callst) call jeveuo(chslst//'.CNSV', 'L', vr=ctv)
        if (callst) call jeveuo(chslst//'.CNSL', 'L', jctl)
!
        do ino = 1, nbno
!           ON VERIFIE QUE LE NOEUD POSSEDE CETTE COMPOSANTE
            ASSERT(zl(jcnl+ino-1))
            if (callst) then
                ASSERT(zl(jctl+ino-1))
            endif 
            zr(jlnsv-1+(ino-1)+1)=cnv(ino)
            zl(jlnsl-1+(ino-1)+1)=.true.
            if (callst) zr(jltsv-1+(ino-1)+1)=ctv(ino)
            if (.not.callst) zr(jltsv-1+(ino-1)+1)= -1.d0
            zl(jltsl-1+(ino-1)+1)=.true.
        end do
!
        call jedetr(chslsn)
        if (callst) call jedetr(chslst)
!
!       CREATE THE FLAG IN THE SD_FISS_XFEM TO MARK THAT THE LEVEL SETS
!       HAVE BEEN GIVEN DIRECTLY (ALL THE DOMAIN INFOS HAVE BEEN LOST)
        call wkvect(fiss//'.CHAMPS.LVS', 'G V L', 1, ibid)
        zl(ibid) = .true.
!
    endif
!
!-----------------------------------------------------------------------
!     REAJUSTEMENT DE LSN (BOOK III 06/02/04) ET LST
!-----------------------------------------------------------------------
!
    if (grille) then
        call xajuls(maiaux, nbmagr, cnslt, cnsln, jcong1,&
                    jcong2, clsm)
    else
        call xajuls(noma, nbma, cnslt, cnsln, jconx1,&
                    jconx2, clsm)
    endif
!
    if (niv .ge. 2) then
        call utmess('I', 'XFEM_37', si=clsm)
    endif
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
!
    call jedema()
end subroutine
