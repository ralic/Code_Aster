subroutine op0077()
    implicit none
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
!
!     OPERATEUR REST_SOUS_STRUC
!
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/excygl.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/harm75.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/recyec.h"
#include "asterfort/recygl.h"
#include "asterfort/refdaj.h"
#include "asterfort/regeec.h"
#include "asterfort/regegl.h"
#include "asterfort/regene.h"
#include "asterfort/regres.h"
#include "asterfort/rehaec.h"
#include "asterfort/rehagl.h"
#include "asterfort/retrec.h"
#include "asterfort/retrgl.h"
#include "asterfort/rsadpa.h"
#include "asterfort/tran77.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
!
    character(len=8) :: k8b, nomres, resin, nomsst, mailsk, mode
    character(len=8) :: k8bid, result, blanc, param(3)
    character(len=16) :: concep, nomcmd, typres, typrep, champ(4)
    character(len=19) :: profno
    character(len=24) :: matgen, numgen, vbl24(1)
    integer :: ioc1,  nbord, i, iord, lpaout(3)
!
!     -----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, ir, ir1, iret, isk, j
    integer :: j2refe, j3refe, jrefn, jrefnb, lmacr, lmodge
    integer ::  n1, n2, nbcham, numsec
    integer, pointer :: ordr(:) => null()
    character(len=8), pointer :: refm(:) => null()
!-----------------------------------------------------------------------
    data k8b/'        '/
    data param/'MODELE','CHAMPMAT','CARAELEM'/
    data vbl24 /'                        '/
!
    call jemarq()
    call infmaj()
    k8bid='        '
    blanc='        '
!
!     -----------------------------------------------------------------
!
!
    call getres(nomres, typres, nomcmd)
!
! --- PHASE DE TEST SUR LES CHAMPS A RESTITUER
    call getvtx(' ', 'NOM_CHAM', nbval=4, vect=champ, nbret=nbcham)
    if (nbcham .lt. 0) then
        call utmess('E', 'ALGORITH9_44')
    else
        do i = 1, nbcham
            do j = i+1, nbcham
                if (champ(i) .eq. champ(j)) then
                    call utmess('E', 'ALGORITH9_30')
                endif
            end do
        end do
    endif
!
!
! --- CREATION DU PROFIL :
!     ---------------------------
    call getvid(' ', 'SQUELETTE', scal=k8b, nbret=ir)
!     --- SI RESTITUTION SUR UNE SQUELETTE, ALORS ATTACHER UN PROF_CHNO
!         AU RESULTAT
    if (ir .eq. 0) then
        profno = '&&OP0077.PROFC.NUME'
    else
        profno = nomres//'.PROFC.NUME'
    endif
! --- CREATION D'UN OBJET REFN DU PROFIL SUR BASE VOLATILE
    call wkvect(profno//'.REFN', 'V V K24', 4, jrefn)
    zk24(jrefn+1)='DEPL_R'
!
!
!
! --- LE RESULTAT EST-IL GENERALISE OU PAS :
!     ---------------------------
    call getvid(' ', 'RESULTAT', nbval=0, nbret=ir)
    if (ir .eq. 0) then
        call getvid(' ', 'RESU_GENE', scal=resin, nbret=ir1)
        call gettco(resin, concep)
    else
!      --- PROJECTION RESULTAT SUR UN SQUELETTE ENRICHI ---
        call getvid(' ', 'SQUELETTE', scal=mailsk, nbret=ibid)
        call getvid(' ', 'RESULTAT', scal=result, nbret=ibid)
        zk24(jrefn)=mailsk
        call getfac('CYCLIQUE', ioc1)
        if (ioc1 .gt. 0) then
            call excygl(nomres, typres, result, mailsk, profno)
            call jeveuo(profno//'.REFN', 'E', jrefnb)
            zk24(jrefnb)=mailsk
            zk24(jrefnb+1)='DEPL_R'
            concep(1:9)='         '
            resin=result
            goto 30
!
        else
            if (typres .eq. 'MODE_MECA') then
                call regres(nomres, mailsk, result, profno)
                call jeveuo(profno//'.REFN', 'E', jrefnb)
                zk24(jrefnb)=mailsk
                zk24(jrefnb+1)='DEPL_R'
                concep(1:9)='         '
                resin=result
                goto 30
!
            else
                call utmess('E', 'ALGORITH9_46')
            endif
        endif
    endif
!
!
! INDICATEUR CALCUL SANS MATRICE GENERALISEE (PROJ_MESU_MODAL)
!      PROMES=.FALSE.
    if ((concep(1:9).eq.'TRAN_GENE') .or. (concep(1:9).eq.'MODE_GENE') .or.&
        (concep(1:9).eq.'HARM_GENE')) then
        call dismoi('REF_RIGI_PREM', resin, 'RESU_DYNA', repk=matgen, arret='C')
        call dismoi('NUME_DDL', resin, 'RESU_DYNA', repk=numgen)
! LE RESU_GENE VIENT DE PROJ_MESU_MODAL
        if ((matgen(1:8).eq.blanc) .and. (numgen(1:8).eq.blanc)) then
!          PROMES=.TRUE.
            typrep=blanc
        else
            if (numgen(1:8) .eq. blanc) then
                call jeveuo(matgen(1:8)//'           .REFA', 'L', j2refe)
                numgen=zk24(j2refe+1)(1:14)
            endif
            call jeveuo(numgen(1:14)//'.NUME.REFN', 'L', j3refe)
            call gettco(zk24(j3refe), typrep)
        endif
    endif
!
!     --- DYNAMIQUE TRANSITOIRE ---
!
    if (concep(1:9) .eq. 'TRAN_GENE') then
!
        if (typrep(1:11) .eq. 'MODELE_GENE') then
            call getvid(' ', 'SQUELETTE', nbval=0, nbret=isk)
            if (isk .eq. 0) then
                call getvtx(' ', 'SOUS_STRUC', scal=nomsst, nbret=ibid)
                call retrec(nomres, resin, nomsst)
            else
                call getvid(' ', 'SQUELETTE', scal=mailsk, nbret=ibid)
                call retrgl(nomres, resin, mailsk, profno)
                call jeveuo(profno//'.REFN', 'E', jrefnb)
                zk24(jrefnb)=mailsk
                zk24(jrefnb+1)='DEPL_R'
            endif
!
!
!
        else if (typrep(1:9).eq.'MODE_GENE') then
            call getvtx(' ', 'SOUS_STRUC', scal=nomsst, nbret=n1)
            call getvid(' ', 'SQUELETTE', scal=mailsk, nbret=n2)
            if ((n1.ne.0.and.n2.ne.0)) then
                call utmess('F', 'ALGORITH9_47')
            endif
            call getvid(' ', 'MODE_MECA', scal=mode, nbret=ibid)
            if (ibid .eq. 0) then
                call utmess('F', 'ALGORITH9_48')
            endif
            call tran77(nomres, typres, resin, mode)
        endif
!
!
!
!     --- CALCUL MODAL PAR SOUS-STRUCTURATION CLASSIQUE ---
!                  OU SANS SOUS-STRUCTURATION
!
    else if (concep(1:9).eq.'MODE_GENE') then
!
! --- CAS DE LA SOUS-STRUCTURATION MODALE
        if (typrep(1:11) .eq. 'MODELE_GENE') then
!
            call getvid(' ', 'SQUELETTE', nbval=0, nbret=isk)
            if (isk .eq. 0) then
                call getvtx(' ', 'SOUS_STRUC', scal=nomsst, nbret=ibid)
                call regeec(nomres, resin, nomsst)
            else
                call getvid(' ', 'SQUELETTE', scal=mailsk, nbret=ibid)
                call regegl(nomres, resin, mailsk, profno)
                call jeveuo(profno//'.REFN', 'E', jrefnb)
                zk24(jrefnb)=mailsk
                zk24(jrefnb+1)='DEPL_R'
            endif
        else
!
!     --- CALCUL MODAL SANS SOUS-STRUCTURATION ---
            call regene(nomres, resin, profno)
        endif
!
!     --- CALCUL MODAL PAR SOUS-STYRUCTURATION CYCLIQUE ---
!
    else if (concep(1:9).eq.'MODE_CYCL') then
        call getvid(' ', 'SQUELETTE', nbval=0, nbret=isk)
        if (isk .eq. 0) then
            call getvis(' ', 'SECTEUR', scal=numsec, nbret=ibid)
            call recyec(nomres, resin, numsec, 'MODE_MECA')
        else
            call getvid(' ', 'SQUELETTE', scal=mailsk, nbret=ibid)
            call recygl(nomres, 'MODE_MECA', resin, mailsk, profno)
            call jeveuo(profno//'.REFN', 'E', jrefnb)
            zk24(jrefnb)=mailsk
            zk24(jrefnb+1)='DEPL_R'
        endif
!
!     --- CALCUL HARMONIQUE PAR SOUS-STRUCTURATION CLASSIQUE ---
!
    else if (concep(1:9).eq.'HARM_GENE') then
!
! --- CAS DE LA SOUS-STRUCTURATION HARMONIQUE
        if (typrep(1:11) .eq. 'MODELE_GENE') then
            call getvid(' ', 'SQUELETTE', nbval=0, nbret=isk)
            if (isk .eq. 0) then
                call getvtx(' ', 'SOUS_STRUC', scal=nomsst, nbret=ibid)
                call rehaec(nomres, resin, nomsst)
            else
                call getvid(' ', 'SQUELETTE', scal=mailsk, nbret=ibid)
                call rehagl(nomres, resin, mailsk, profno)
                call jeveuo(profno//'.REFN', 'E', jrefnb)
                zk24(jrefnb)=mailsk
                zk24(jrefnb+1)='DEPL_R'
            endif
!
        else if (typrep(1:9).eq.'MODE_GENE') then
            call getvtx(' ', 'SOUS_STRUC', scal=nomsst, nbret=n1)
            call getvid(' ', 'SQUELETTE', scal=mailsk, nbret=n2)
            if ((n1.ne.0.and.n2.ne.0)) then
                call utmess('F', 'ALGORITH9_47')
            endif
            call getvid(' ', 'MODE_MECA', scal=mode, nbret=ibid)
            if (ibid .eq. 0) then
                call utmess('F', 'ALGORITH9_48')
            endif
            call harm75(nomres, typres, resin, mode)
        endif
!
    endif
!
 30 continue
!
! --- STOCKAGE
    call gettco(resin, concep)
    if ((concep(1:9).ne.'TRAN_GENE') .and. (concep(1:9).ne.'MODE_CYCL') .and.&
        (concep(1:9).ne.'HARM_GENE')) then
        call jeveuo(nomres//'           .ORDR', 'L', vi=ordr)
        call jelira(nomres//'           .ORDR', 'LONUTI', nbord)
        call jelira(nomres//'           .ORDR', 'LONUTI', nbord)
!
        call getvid(' ', 'SQUELETTE', nbval=0, nbret=isk)
        if (isk .eq. 0) then
            call getvtx(' ', 'SOUS_STRUC', scal=nomsst, nbret=ibid)
!
!-- RECUPERATION DU MACRO ELEMENT ASSOCIE A LA SOUS STRUCTURE
!           call dismoi('F', 'REF_RIGI_PREM', resin, 'RESU_DYNA', ibid, raide, ir)
!           call jeveuo(raide(1:8)//'           .REFA', 'L', vk24=nlnume)
!           call jeveuo(nlnume(2)(1:14)//'.NUME.REFN', 'L', lmodge)
!
            call dismoi('NUME_DDL', resin, 'RESU_DYNA', repk=numgen)
            call jeveuo(numgen(1:14)//'.NUME.REFN', 'L', lmodge)
            call jenonu(jexnom(zk24(lmodge)(1:8)//'      .MODG.SSNO', nomsst), iret)
            call jeveuo(jexnum(zk24(lmodge)(1:8)//'      .MODG.SSME', iret), 'L', lmacr)
!-- RECUPERATION DES INFOS CARA_ELEM / MATER / MODELE POUR LES SST
!-- DANS LE .REFM DANS LE MACRO ELEMENT CORRESPONDANT
            call jeveuo(zk8(lmacr)//'.REFM', 'L', vk8=refm)
            do iord = 1, nbord
                call rsadpa(nomres, 'E', 3, param, ordr(iord),&
                            0, tjv=lpaout, styp=k8b)
                zk8(lpaout(1))=refm(1)
                zk8(lpaout(2))=refm(3)
                zk8(lpaout(3))=refm(4)
            end do
        endif
!
    endif
!
!     -- CREATION DE L'OBJET .REFD SI NECESSAIRE:
!     -------------------------------------------
    call jeexin(nomres//'           .REFD', iret)
    if (iret .eq. 0) call refdaj(' ', nomres, -1, profno, 'INIT',&
                                 ' ', iret)
!
    call jedema()
end subroutine
