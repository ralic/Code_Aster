subroutine regegl(nomres, resgen, mailsk, profno)
    implicit  none
#include "jeveux.h"
!
#include "asterc/getvis.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/genugl.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/matrot.h"
#include "asterfort/mgutdm.h"
#include "asterfort/rotchm.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesg.h"
#include "asterfort/vtcrea.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomres, resgen, mailsk
    character(len=19) :: profno
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  BUT : < RESTITUTION GENERALISEE GLOBALE >
!
!  RESTITUER EN BASE PHYSIQUE SUR UN MAILLAGE SQUELETTE LES RESULTATS
!  ISSUS DE LA SOUS-STRUCTURATION GENERALE.
!  LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "MODE_MECA"
!
!-----------------------------------------------------------------------
!
! NOMRES  /I/ : NOM K8 DU CONCEPT MODE_MECA RESULTAT
! RESGEN  /I/ : NOM K8 DU RESULTAT GENERALISE AMONT
! MAILSK  /I/ : NOM K8 DU MAILLAGE SQUELETTE SUPPORT
! PROFNO  /I/ : NOM K19 DU PROF_CHNO DU SQUELETTE
!
!
!
!
!
    integer :: i, iad, iar, ibid, idep, ieq, ier, iord, iret, j, jbid, k, l
    integer :: ldnew, llchab, llchol, llind, lliner, llinsk, llmass, i1, k1
    integer :: llnueq, llors, llprs, llrot, lrefe, ltrotx, ltroty, ltrotz, ltvec
    integer :: ltype, nbbas, nbcmp, nbcou, nbmas, nbmax, nbmod, nbnot, nbsst
    integer :: neq, neqs, nno, numo, nutars, llref1, llref2, llref3, elim, lmoet
    integer :: neqet, lmapro, neqred, lsilia, numsst, lsst
    integer :: iadpar(9)
    integer :: vali(2)
    real(kind=8) :: compx, compy, compz, efmasx, efmasy, efmasz, freq, genek
    real(kind=8) :: genem, mat(3, 3), omeg2, rbid
    character(len=8) :: basmod, macrel, modgen, soutr, kbid
    character(len=16) :: depl, nompar(9)
    character(len=19) :: raid, numddl, numgen, chamne
    character(len=24) :: crefe(2), chamol, chamba, indirf, seliai, sizlia, sst
    character(len=24) :: valk, nomsst
    complex(kind=8) :: cbid
    character(len=1) :: k1bid
    integer :: iarg
!
!-----------------------------------------------------------------------
    data depl   /'DEPL            '/
    data soutr  /'&SOUSSTR'/
    data nompar /'FREQ','RIGI_GENE','MASS_GENE','OMEGA2','NUME_MODE',&
     &             'MASS_EFFE_DX','MASS_EFFE_DY','MASS_EFFE_DZ',&
     &             'TYPE_MODE'/
!-----------------------------------------------------------------------
!
    call jemarq()
!
    indirf='&&REGEGL.INDIR.SST'
!
!-----ECRITURE DU TITRE-------------------------------------------------
!
    call titre()
!
!-----VERIF SQUELETTE---------------------------------------------------
!
    call jeexin(mailsk//'.INV.SKELETON', iret)
    if (iret .eq. 0) then
        valk = mailsk
        call u2mesg('F', 'ALGORITH14_27', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
    call jeveuo(mailsk//'.INV.SKELETON', 'L', llinsk)
!
!-----RECUPERATION DU MODELE GENERALISE--------------------------------
!
    call jeveuo(resgen//'           .REFD', 'L', llref1)
    raid=zk24(llref1)
    call jelibe(resgen//'           .REFD')
!
    call jeveuo(raid//'.REFA', 'L', llref2)
    numgen(1:14)=zk24(llref2+1)
    numgen(15:19)='.NUME'
    call jelibe(raid//'.REFA')
!
    call jeveuo(numgen//'.REFN', 'L', llref3)
    modgen=zk24(llref3)
    call jelibe(numgen//'.REFN')
!
    call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst, k1bid)
    kbid='  '
    call mgutdm(modgen, kbid, 1, 'NB_CMP_MAX', nbcmp,&
                kbid)
!
!-----RECUPERATION DES ROTATIONS----------------------------------------
!
    call wkvect('&&REGEGL.ROTX', 'V V R', nbsst, ltrotx)
    call wkvect('&&REGEGL.ROTY', 'V V R', nbsst, ltroty)
    call wkvect('&&REGEGL.ROTZ', 'V V R', nbsst, ltrotz)
    do 15 i = 1, nbsst
        call jeveuo(jexnum(modgen//'      .MODG.SSOR', i), 'L', llrot)
        zr(ltrotz+i-1)=zr(llrot)
        zr(ltroty+i-1)=zr(llrot+1)
        zr(ltrotx+i-1)=zr(llrot+2)
15  end do
!
!-----CREATION DU PROF-CHAMNO-------------------------------------------
!
    call genugl(profno, indirf, modgen, mailsk)
    call jelira(profno//'.NUEQ', 'LONMAX', neq, k1bid)
!
!-----RECUPERATION DU NOMBRE DE NOEUDS----------------------------------
!
    call dismoi('F', 'NB_NO_MAILLA', mailsk, 'MAILLAGE', nbnot,&
                kbid, iret)
!
!-----RECUPERATION DE LA BASE MODALE------------------------------------
!
    crefe(1)=mailsk
    crefe(2)=profno
!
!-----RECUPERATION NOMBRE DE MODES PROPRES CALCULES---------------------
!
    call rsorac(resgen, 'LONUTI', ibid, rbid, kbid,&
                cbid, rbid, kbid, nbmod, 1,&
                ibid)
!
!
! --- ON RESTITUE SUR TOUS LES MODES OU SUR QUELQUES MODES:
!
    call getvis(' ', 'NUME_ORDRE', 1, iarg, 0,&
                ibid, nno)
    if (nno .ne. 0) then
        nbmod = -nno
        call wkvect('&&REGEGL.NUME', 'V V I', nbmod, jbid)
        call getvis(' ', 'NUME_ORDRE', 1, iarg, nbmod,&
                    zi(jbid), nno)
    else
        call wkvect('&&REGEGL.NUME', 'V V I', nbmod, jbid)
        do 2 i = 1, nbmod
            zi(jbid+i-1) = i
 2      continue
    endif
!
!-----ALLOCATION STRUCTURE DE DONNEES RESULTAT--------------------------
!
    call rscrsd('G', nomres, 'MODE_MECA', nbmod)
!
!-- ON TESTE SI ON A EU RECOURS A L'ELIMINATION
!
!
!      SELIAI= '&&'//NUMGEN(1:8)//'PROJ_EQ_LIAI'
!      SIZLIA='&&'//NUMGEN(1:8)//'VECT_SIZE_SS'
!      SST=    '&&'//NUMGEN(1:8)//'VECT_NOM_SS'
    seliai=numgen(1:14)//'.ELIM.BASE'
    sizlia=numgen(1:14)//'.ELIM.TAIL'
    sst=   numgen(1:14)//'.ELIM.NOMS'
!
    call jeexin(seliai, elim)
!
    if (elim .ne. 0) then
        neqet=0
        call jeveuo(numgen//'.NEQU', 'L', ibid)
        neqred=zi(ibid)
        nomsst=modgen//'      .MODG.SSNO'
        call jeveuo(seliai, 'L', lmapro)
        call jeveuo(sizlia, 'L', lsilia)
        call jeveuo(sst, 'L', lsst)
        do 10 i = 1, nbsst
            neqet=neqet+zi(lsilia+i-1)
10      continue
        call wkvect('&&MODE_ETENDU_REST_ELIM', 'V V R', neqet, lmoet)
    endif
!
!C
!CC---RESTITUTION PROPREMENT DITE---------------------------------------
!C
!
    call jeveuo(numgen//'.NUEQ', 'L', llnueq)
    call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
    call jeveuo(jexnum(numgen//'.ORIG', ibid), 'L', llors)
    call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
    call jeveuo(jexnum(numgen//'.PRNO', ibid), 'L', llprs)
!
!  BOUCLE SUR LES MODES A RESTITUER
!
    do 20 i = 1, nbmod
        iord = zi(jbid+i-1)
!
!  REQUETTE NOM ET ADRESSE CHAMNO GENERALISE
!
        call dcapno(resgen, depl, iord, chamol)
        call jeveuo(chamol, 'L', llchol)
!
!-- SI ELIMINATION, ON RESTITUE D'ABORD LES MODES GENERALISES
        if (elim .ne. 0) then
            do 21 i1 = 1, neqet
                zr(lmoet+i1-1)=0.d0
                do 31 k1 = 1, neqred
                    zr(lmoet+i1-1)=zr(lmoet+i1-1)+ zr(lmapro+(k1-1)*&
                    neqet+i1-1)* zr(llchol+k1-1)
31              continue
21          continue
            llchol=lmoet
        endif
!
!  REQUETTE NOM ET ADRESSE NOUVEAU CHAMNO
!
        call rsexch(' ', nomres, depl, i, chamne,&
                    ier)
        call vtcrea(chamne, crefe, 'G', 'R', neq)
        call jeveuo(chamne//'.VALE', 'E', ldnew)
!
        call rsadpa(resgen, 'L', 8, nompar, iord,&
                    0, iadpar, kbid)
        freq = zr(iadpar(1))
        genek = zr(iadpar(2))
        genem = zr(iadpar(3))
        omeg2 = zr(iadpar(4))
        numo = zi(iadpar(5))
        efmasx = 0.d0
        efmasy = 0.d0
        efmasz = 0.d0
!
!  BOUCLE SUR LES SOUS-STRUCTURES
!
        do 30 k = 1, nbsst
            call jeexin(jexnum(indirf, k), iret)
!
!
!  TEST SI LA SST GENERE DES DDL GLOBAUX
!
            if (iret .ne. 0) then
                kbid='  '
                if (elim .ne. 0) then
                    call jenonu(jexnom(nomsst, zk8(lsst+k-1)), numsst)
                    ieq=0
                    do 41 i1 = 1, numsst-1
                        ieq=ieq+zi(lsilia+i1-1)
41                  continue
                else
                    numsst=k
!  RECUPERATION DU NUMERO TARDIF DE LA SST
                    do 40 j = 1, nbsst
                        if (zi(llors+j-1) .eq. numsst) nutars=j
40                  continue
                    ieq=zi(llprs+(nutars-1)*2)
                endif
!
                call mgutdm(modgen, kbid, k, 'NOM_BASE_MODALE', ibid,&
                            basmod)
                call jeveuo(basmod//'           .REFD', 'L', lrefe)
                call jeveuo(zk24(lrefe+4)(1:8)//'.IDC_TYPE', 'L', ltype)
                if (zk8(ltype) .eq. 'AUCUN') then
                    vali (1) = k
                    vali (2) = k
                    call u2mesg('A', 'ALGORITH14_28', 0, ' ', 2,&
                                vali, 0, 0.d0)
                endif
                call mgutdm(modgen, kbid, k, 'NOM_MACR_ELEM', ibid,&
                            macrel)
                call jeveuo(macrel//'.MAEL_MASS_VALE', 'L', llmass)
                call jelira(macrel//'.MAEL_MASS_VALE', 'LONMAX', nbmax, k1bid)
                call jelira(macrel//'.MAEL_MASS_VALE', 'LONUTI', nbmas, k1bid)
                call jeveuo(macrel//'.MAEL_INER_VALE', 'L', lliner)
!
!           --- CALCUL DE LA MATRICE DE ROTAION ---
                call jeveuo(jexnum(modgen//'      .MODG.SSOR', k), 'L', llrot)
                call matrot(zr(llrot), mat)
!
                call dismoi('F', 'NB_MODES_TOT', basmod, 'RESULTAT', nbbas,&
                            kbid, ier)
                kbid='  '
                call mgutdm(modgen, kbid, k, 'NOM_NUME_DDL', ibid,&
                            numddl)
                call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neqs,&
                            kbid, iret)
                call wkvect('&&REGEGL.TRAV', 'V V R', neqs, ltvec)
!
!  BOUCLE SUR LES MODES PROPRES DE LA BASE
!
                do 50 j = 1, nbbas
                    call dcapno(basmod, depl, j, chamba)
                    call jeveuo(chamba, 'L', llchab)
!
!  BOUCLE SUR LES EQUATIONS PHYSIQUES
!
                    if (elim .ne. 0) then
                        iad=llchol+ieq+j-1
                    else
                        iad=llchol+zi(llnueq+ieq+j-2)-1
                    endif
!
!-- DANS LE CAS ELIM, CHANGER LE IAD, le ZI(LLNUEQ EST PAS BON)
!
!           --- CALCUL DES MASSES EFFECTIVES ---
                    compx = zr(lliner+j-1)
                    compy = zr(lliner+nbbas+j-1)
                    compz = zr(lliner+2*nbbas+j-1)
!             --- UTILISATION DE MAT TRANSPOSEE (TRANSFORMATION INVERSE)
                    efmasx = efmasx + zr(iad)*(compx*mat(1,1) + compy* mat(2,1) + compz*mat(3,1))
                    efmasy = efmasy + zr(iad)*(compx*mat(1,2) + compy* mat(2,2) + compz*mat(3,2))
                    efmasz = efmasz + zr(iad)*(compx*mat(1,3) + compy* mat(2,3) + compz*mat(3,3))
!
!  BOUCLE SUR LES DDL DE LA BASE
!
                    do 60 l = 1, neqs
                        zr(ltvec+l-1)=zr(ltvec+l-1)+zr(llchab+l-1)*&
                        zr(iad)
60                  continue
!
                    call jelibe(chamba)
50              continue
                call jeveuo(jexnum(indirf, k), 'L', llind)
                call jelira(jexnum(indirf, k), 'LONMAX', nbcou, k1bid)
                nbcou=nbcou/2
                do 70 l = 1, nbcou
                    idep=zi(llind+(l-1)*2)
                    iar=zi(llind+(l-1)*2+1)
                    zr(ldnew+iar-1)=zr(ltvec+idep-1)
70              continue
                call jelibe(jexnum(indirf, k))
                call jedetr('&&REGEGL.TRAV')
            endif
30      continue
!
        efmasx = efmasx*efmasx/genem
        efmasy = efmasy*efmasy/genem
        efmasz = efmasz*efmasz/genem
        call rsnoch(nomres, depl, i)
        call rsadpa(nomres, 'E', 9, nompar, i,&
                    0, iadpar, kbid)
        zr(iadpar(1)) = freq
        zr(iadpar(2)) = genek
        zr(iadpar(3)) = genem
        zr(iadpar(4)) = omeg2
        zi(iadpar(5)) = numo
        zr(iadpar(6)) = efmasx
        zr(iadpar(7)) = efmasy
        zr(iadpar(8)) = efmasz
        zk16(iadpar(9)) = 'MODE_DYN'
!
        call jelibe(chamol)
!
!  ROTATION DU CHAMPS AUX NOEUDS
!
        call rotchm(profno, zr(ldnew), zr(ltrotz), nbsst, zi(llinsk),&
                    nbnot, nbcmp, 3)
        call rotchm(profno, zr(ldnew), zr(ltroty), nbsst, zi(llinsk),&
                    nbnot, nbcmp, 2)
        call rotchm(profno, zr(ldnew), zr(ltrotx), nbsst, zi(llinsk),&
                    nbnot, nbcmp, 1)
20  end do
!
! --- MENAGE
    call jedetr('&&REGEGL.ROTX')
    call jedetr('&&REGEGL.ROTY')
    call jedetr('&&REGEGL.ROTZ')
!
    call jedetr('&&MODE_ETENDU_REST_ELIM')
    call jedetr(indirf)
    call jelibe(numgen//'.NUEQ')
    call jedetr('&&REGEGL.NUME')
!
    call jedema()
end subroutine
