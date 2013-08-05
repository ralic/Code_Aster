subroutine op0075()
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!
!     OPERATEUR REST_GENE_PHYS
!
! ----------------------------------------------------------------------
#include "jeveux.h"
!     ------------------------------------------------------------------
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/harm75.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/regene.h"
#include "asterfort/rsadpa.h"
#include "asterfort/tran75.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: k8bid, nomres, resin, mode, blanc8, param(3)
    character(len=16) :: concep, nomcmd, typres, typrep, champ(4)
    character(len=19) :: profno
    character(len=24) :: matgen, numgen, basemo
    logical :: prsimp
    integer :: jord, nbord, i, iord, lpain(3), lpaout(3), iarg, ibid, ir1
    integer :: j, j1refe, j2refe, j3refe, jrefn, n1, nbcham
!     ------------------------------------------------------------------
    call jemarq()
    call infmaj()
!     ------------------------------------------------------------------
    data k8bid/'        '/
    data param/'MODELE','CHAMPMAT','CARAELEM'/
    blanc8 = '        '
!     -----------------------------------------------------------------
    call getres(nomres, typres, nomcmd)
!
!     --- PHASE DE TEST SUR LES CHAMPS A RESTITUER
    call getvtx(' ', 'NOM_CHAM', 1, iarg, 4,&
                champ, nbcham)
    if (nbcham .lt. 0) then
        call u2mess('E', 'ALGORITH9_44')
    else
        do 20 i = 1, nbcham
            do 10 j = i+1, nbcham
                if (champ(i) .eq. champ(j)) then
                    call u2mess('E', 'ALGORITH9_30')
                endif
10          continue
            if (champ(i) .eq. 'ACCE_ABSOLU') then
                call getvid(' ', 'ACCE_MONO_APPUI', 1, iarg, 1,&
                            k8bid, n1)
                if (n1 .eq. 0) then
                    call u2mess('E', 'ALGORITH9_45')
                endif
            endif
20      continue
    endif
!
!     --- CREATION DU .REFN DU PROFIL :
    profno = '&&OP0075'//'.PROFC.NUME'
!
    call wkvect(profno(1:19)//'.REFN', 'V V K24', 4, jrefn)
    zk24(jrefn+1)='DEPL_R'
!
    call getvid(' ', 'RESU_GENE', 1, iarg, 1,&
                resin, ir1)
    call gettco(resin, concep)
!
!     --- INDICATEUR : 1) CALCUL CLASSIQUE AVEC UNE SIMPLE PROJECTION
!       -           OU 2) SANS MATRICE GENERALISEE (PROJ_MESU_MODAL)
    prsimp=.true.
!
    call jeveuo(resin//'           .REFD', 'L', j1refe)
    matgen=zk24(j1refe)
    numgen=zk24(j1refe+3)
    basemo=zk24(j1refe+4)
!
!     --- LE RESU_GENE NE VIENT PAS DE PROJ_MESU_MODAL
    if ((matgen(1:8).ne.blanc8) .or. (numgen(1:8).ne.blanc8)) then
        typrep = 'MODE_MECA'
        if (basemo(1:8) .ne. blanc8) call gettco(basemo, typrep)
!       --- LA BASE REFERENCEE DANS LE .REFD N'EST PAS UN MODE_MECA
        if (typrep(1:9) .ne. 'MODE_MECA') then
            prsimp = .false.
!         --- CHERCHER ALORS LE NUME_DDL_GENE POUR Y TROUVER DES INFOS
!           - SUR UN POTENTIEL MODE_GENE (CAS D'UNE DOUBLE-RESTITUTION)
            if (numgen(1:8) .eq. blanc8) then
!           --- PAS D'ENTREE DANS LE .REFD => CHERCHER DANS LA MATRICE K
                call jeveuo(matgen(1:8)//'           .REFA', 'L', j2refe)
                numgen=zk24(j2refe+1)(1:14)
            endif
            call jeveuo(numgen(1:14)//'.NUME.REFN', 'L', j3refe)
            call gettco(zk24(j3refe), typrep)
        endif
    endif
!
!
!     --- DYNAMIQUE TRANSITOIRE ---
    if (concep(1:9) .eq. 'TRAN_GENE') then
        if (prsimp) then
!         --- SIMPLE RESTITUTION => APPELER TRAN75 AVEC MODE=BLANC8
            call tran75(nomres, typres, resin, blanc8)
!
        else if (typrep(1:9).eq.'MODE_GENE') then
!         --- RECUPERER LA BASE MODALE POUR DOUBLE RESTITUTION
            call getvid(' ', 'MODE_MECA', 1, iarg, 1,&
                        mode, ibid)
            if (ibid .eq. 0) call u2mess('F', 'ALGORITH9_48')
!
            call tran75(nomres, typres, resin, mode)
!
        else
!     REMARQUE 1: BLINDAGE
!         --- IMPOSSIBILE DE DETERMINER LE TYPE DE RESTITUTION A PARTIR
!           - DE LA SD_DYNA_GENE, LA SD A PROBABLEMENT ETE MAL DEFINIE
!           - A LA BASE. ON ARRETE LE CALCUL.
            ASSERT(.false.)
        endif
!
!     --- CALCUL MODAL SANS SOUS-STRUCTURATION
    else if (concep(1:9).eq.'MODE_GENE') then
        call regene(nomres, resin, profno)
!
!     --- CALCUL HARMONIQUE
    else if (concep(1:9).eq.'HARM_GENE') then
        if (prsimp) then
!         --- SANS SOUS STRUCTURATION
            call harm75(nomres, typres, resin, nomcmd, blanc8)
!
        else if (typrep(1:9).eq.'MODE_GENE') then
!         --- AVEC SOUS STRUCTURATION, RECUPERER LA BASE MODALE
            call getvid(' ', 'MODE_MECA', 1, iarg, 1,&
                        mode, ibid)
            if (ibid .eq. 0) call u2mess('F', 'ALGORITH9_48')
!
            call harm75(nomres, typres, resin, nomcmd, mode)
!
        else
!         --- BLINDAGE : VOIR REMARQUE 1
            ASSERT(.false.)
        endif
    endif
!
!     --- STOCKAGE DES RESULTATS
    call gettco(resin, concep)
    if ((concep(1:9).eq.'MODE_GENE')) then
        call jeveuo(nomres//'           .ORDR', 'L', jord)
        call jelira(nomres//'           .ORDR', 'LONUTI', nbord, k8bid)
        do 50 iord = 1, nbord
            call rsadpa(resin, 'L', 3, param, zi(jord+iord-1),&
                        0, lpain, k8bid)
            call rsadpa(nomres, 'E', 3, param, zi(jord+iord-1),&
                        0, lpaout, k8bid)
            do 40 i = 1, 3
                zk8(lpaout(i))=zk8(lpain(i))
40          continue
50      continue
    endif
!
    call jedema()
end subroutine
