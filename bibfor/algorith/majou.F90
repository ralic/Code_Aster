subroutine majou(model, modmec, solveu, num, nu,&
                 ma, mate, moint, ndble, icor,&
                 tabad)
    implicit none
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
!     AUTEUR : G.ROUSSEAU
!     ROUTINE REALISANT ,
!     A PARTIR D'UN MODELE GENERALISE, LE CALCUL MASSE AJOUTE.
!     IN: K2 : MODEL : CHARACTER TRADUISANT LA DIMENSION DU FLUIDE
!     IN: K8 : MODMEC : NOM DU CONCEPT MODE_MECA RESTITUE SUR MAILLAGE
!              SQUELETTE
!     IN : K14 : NU :NUMEROTATION ASSOCIEE AU MODELE FLUIDE
!     IN : K14 : NUM :NUMEROTATION ASSOCIEE AU MODELE INTERFACE
!     IN : K8 : MA : MATRICE DE RAIDEUR DU FLUIDE
!     IN : K8 : MOINT: MODELE INTERFACE
!     IN : I : IADX,IADY,IADZ : ADRESSES DES VECTEURS DE NOMS
!              DES CHAMNOS ASSOCIES PAR CMP DE DEPLACEMENT ET
!              PAR MODE D UNE SOUS STRUCTURE DONNEE D INDICE ISST
!     IN : I : IADRP : ADRESSE DU TABLEAU D ADRESSES DES VECTEURS
!              CONTENANT LES NOMS DES CHAMPS DE PRESSION
!     IN : I : ICOR(2) : TABLEAU CONTENANT LES ADRESSES
!                        JEVEUX DE TABLEAUX D'ENTIER
!              INDIQUANT LA CORRESPONDANCE ENTRE NOEUDS DE STRUCTURE
!              ET DE FLUIDE
!     IN: I: NDBLE: INDICATEUR DE RECHERCHE DE NOEUDS DOUBLES
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/alimrs.h"
#include "asterfort/calflu.h"
#include "asterfort/chnucn.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvis.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/prstoc.h"
#include "asterfort/resoud.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcmbl.h"
#include "asterfort/wkvect.h"
    integer :: nbvale, nbrefe, nbdesc, ibid, nbid, nbmode, ilires
    integer :: i, icor(2), ndble, tabad(5), jj, kk
    real(kind=8) :: const(2)
    character(len=1) :: typech(2), typcst(2)
    character(len=2) :: model
    character(len=6) :: chaine
    character(len=8) :: nomch(2)
    character(len=8) :: moint, ma, k8bid, maflui, modmec
    character(len=8) :: mailla
    character(len=14) :: nu, num
    character(len=19) :: chcomb, vestoc
    character(len=19) :: vesolx, vesoly, vepr, vesolz, tampon, chcmb2
    character(len=19) :: chflu, chamnx, chamny, chamnz, solveu
    character(len=24) :: nomcha
    character(len=*) :: mate
    complex(kind=8) :: cbid
    integer :: nbsel, idsel, n15, n16, ii, vali, tmod(1)
    character(len=20) :: tempor
!
! -----------------------------------------------------------------
!-----------------------------------------------------------------
! ON RECUPERE LE NOMBRE DE MODES DANS LE MODE_MECA
! DEFINI
!-----------------------------------------------------------------------
    integer :: iadirg, iadpr, iadx, iady, iadz, idesp, ierd
    integer :: irefp, iret, ivalp, jchflu, jordr, jpara, nbnumo
!
    real(kind=8) :: bid, ebid
!-----------------------------------------------------------------------
    call jemarq()
!
!     INITIALISATIONS
    tempor = '&&MAJOU.NUMODE'
    vepr = '&&MAJOU.VEPR'
    chcomb = '&&CHCOMB'
    chcmb2 = '&&CHCMB2'
    chflu = '&&MAJOU.CHFLU'
    vesolx = '&&VEMAJX'
    vesoly = '&&VEMAJY'
    vesolz = '&&VEMAJZ'
    chamnx = '&&MAJOU.CHAMNX'
    chamny = '&&MAJOU.CHAMNY'
    chamnz = '&&MAJOU.CHAMNZ'
    iadz = 1
!
    call rsorac(modmec, 'LONUTI', ibid, bid, k8bid,&
                cbid, ebid, 'ABSOLU', tmod, 1,&
                nbid)
    nbmode=tmod(1)
!
!
! CREATION DE VECTEURS CONTENANT LES NOMS DES VECTEURS DE CHAMP AUX
! NOEUDS DE DEPLACEMENTS SUIVANT OX  OY  OZ AINSI QUE LE CHAMP DE
! PRESSION ASSOCIE A CHAQUE MODE PROPRE
!
    call jecreo('&&MAJOU.TXSTO', 'V V K24')
    call jeecra('&&MAJOU.TXSTO', 'LONMAX', nbmode)
    call jeecra('&&MAJOU.TXSTO', 'LONUTI', nbmode)
    call jeveut('&&MAJOU.TXSTO', 'E', iadx)
    call jecreo('&&MAJOU.TYSTO', 'V V K24')
    call jeecra('&&MAJOU.TYSTO', 'LONMAX', nbmode)
    call jeecra('&&MAJOU.TYSTO', 'LONUTI', nbmode)
    call jeveut('&&MAJOU.TYSTO', 'E', iady)
    if (model .eq. '3D') then
        call jecreo('&&MAJOU.TZSTO', 'V V K24')
        call jeecra('&&MAJOU.TZSTO', 'LONMAX', nbmode)
        call jeecra('&&MAJOU.TZSTO', 'LONUTI', nbmode)
        call jeveut('&&MAJOU.TZSTO', 'E', iadz)
    endif
    call jecreo('&&MAJOU.PRES', 'V V K24')
    call jeecra('&&MAJOU.PRES', 'LONMAX', nbmode)
    call jeecra('&&MAJOU.PRES', 'LONUTI', nbmode)
    call jeveut('&&MAJOU.PRES', 'E', iadpr)
!
    call jecreo('&&TABIRG', 'V V I')
    call jeecra('&&TABIRG', 'LONMAX', nbmode)
    call jeecra('&&TABIRG', 'LONUTI', nbmode)
    call jeveut('&&TABIRG', 'E', iadirg)
!
    do 6 i = 1, nbmode
        zi(iadirg+i-1)=i
 6  continue
!
! FORMATION DU TABLEAU DES ADRESSES DES TABLEAUX
!
    tabad(1)=iadx
    tabad(2)=iady
    tabad(3)=iadz
    tabad(4)=iadpr
    tabad(5)=iadirg
!
! RECUPERATION DES NOMS DE MAILLAGES
    call rsexch('F', modmec, 'DEPL', 1, nomcha,&
                iret)
    call dismoi('F', 'NOM_MAILLA', nomcha(1:19), 'CHAM_NO', ibid,&
                mailla, ierd)
    call dismoi('F', 'NOM_MAILLA', moint, 'MODELE', ibid,&
                maflui, ierd)
!
! RECUPERATION DES MODES SELECTIONNES
!
    call getvis(' ', 'NUME_MODE_MECA', nbval=0, nbret=n15)
    nbsel=-1*n15
    if (nbsel .gt. 0) then
        call wkvect(tempor, 'V V I', nbsel, idsel)
        call getvis(' ', 'NUME_MODE_MECA', nbval=nbsel, vect=zi(idsel), nbret=n16)
    endif
!
! VERIFICATION QUE LES NUMEROS DES MODES DONNES PAR L'USER
! CORRESPONDENT A DES NUMEROS EXISTANTS DANS LES LES MODES
! UTILISES
!      CALL JEVEUO(MODMEC//'           .NUMO','L',INUMO)
    call jelira(modmec//'           .ORDR', 'LONMAX', nbnumo)
    call jeveuo(modmec//'           .ORDR', 'L', jordr)
    do 100 jj = 1, nbsel
        do 200 kk = 1, nbnumo
            call rsadpa(modmec, 'L', 1, 'NUME_MODE', zi(jordr-1+kk),&
                        0, sjv=jpara, styp=k8bid)
            if (zi(idsel+jj-1) .eq. zi(jpara)) goto 100
200      continue
        vali = zi(idsel+jj-1)
        call utmess('F', 'ALGORITH13_35', si=vali)
100  continue
!
!
!
!
! BOUCLE SUR LE NOMBRES DE MODES
!
    do 1 ilires = 1, nbmode
!
!
! SORTIE DE BOUCLE POUR LES MODES NON-SELECTIONNES
        if (nbsel .gt. 0) then
            do 2 ii = 0, nbsel-1
                if (ilires .eq. zi(idsel+ii)) goto 22
 2          continue
            goto 1
22          continue
        endif
!
!
        call rsexch('F', modmec, 'DEPL', ilires, nomcha,&
                    iret)
        call alimrs(mate, mailla, maflui, moint, ndble,&
                    num, nomcha(1:19), chamnx, 'DX', icor)
        call alimrs(mate, mailla, maflui, moint, ndble,&
                    num, nomcha(1:19), chamny, 'DY', icor)
        if (model .eq. '3D') then
            call alimrs(mate, mailla, maflui, moint, ndble,&
                        num, nomcha(1: 19), chamnz, 'DZ', icor)
        endif
!
!
!
!---------ON TRANSPORTE CE MODE TRANSFORME EN TEMPERATURE
!-----SUR LES CONTOURS DE LA INTERFACE FLUIDE
!
        typcst(1) ='R'
        typcst(2) ='R'
!
        const(1) =1.0d0
        const(2) =1.0d0
!
        typech(1) ='R'
        typech(2) ='R'
!
        nomch(1) = vesolx(1:8)
        nomch(2) = vesoly(1:8)
!
!
        call calflu(chamnx, moint, mate, num, vesolx,&
                    nbdesc, nbrefe, nbvale, 'X')
        call calflu(chamny, moint, mate, num, vesoly,&
                    nbdesc, nbrefe, nbvale, 'Y')
!
        vestoc='&&MAJOU.TPXSTO'
        call prstoc(chamnx, vestoc, ilires, ilires, iadx,&
                    nbvale, nbrefe, nbdesc)
!
        vestoc='&&MAJOU.TPYSTO'
        call prstoc(chamny, vestoc, ilires, ilires, iady,&
                    nbvale, nbrefe, nbdesc)
!
        if (model .eq. '3D') then
            call calflu(chamnz, moint, mate, num, vesolz,&
                        nbdesc, nbrefe, nbvale, 'Z')
            vestoc='&&MAJOU.TPZSTO'
            call prstoc(chamnz, vestoc, ilires, ilires, iadz,&
                        nbvale, nbrefe, nbdesc)
        endif
!
!---ON RECOMBINE LES DEUX (TROIS)CHAMPS AUX NOEUDS DE TEMP ET ON CALCULE
!-----LE FLUX FLUIDE TOTAL.....
!
!
        call vtcmbl(2, typcst, const, typech, nomch,&
                    'R', chcomb)
!
        if (model .eq. '3D') then
!
            typcst(1) ='R'
            typcst(2) ='R'
!
            const(1) =1.0d0
            const(2) =1.0d0
!
            typech(1) ='R'
            typech(2) ='R'
!
            nomch(1) = chcomb(1:8)
            nomch(2) = vesolz(1:8)
!
            call vtcmbl(2, typcst, const, typech, nomch,&
                        'R', chcmb2)
!
        endif
!
        if (model .eq. '3D') then
            tampon=chcmb2
        else
            tampon=chcomb
        endif
!
        call chnucn(tampon, nu, 0, k8bid, 'V',&
                    chflu)
!
!----ON RESOUT L EQUATION DE LAPLACE
!
        call jeveuo(chflu//'.VALE', 'E', jchflu)
        call resoud(ma, ' ', solveu, ' ', 1,&
                    ' ', ' ', ' ', zr(jchflu), [cbid],&
                    ' ', .true., 0, iret)
!
!
!--------ON REPLONGE LA PRESSION SUR L INTERFACE
!-----------------QU 'ON STOCKE
!
        call chnucn(chflu, num, 0, k8bid, 'V',&
                    vepr)
!
        vestoc= '&&MAJOU.VESTOC'
        call prstoc(vepr, vestoc, ilires, ilires, iadpr,&
                    nbvale, nbrefe, nbdesc)
 1  continue
!
! CREATION DE TABLEAUX NULS POUR LA PRESSION ET LES
! DEPLACEMENTS DES MODES NON-SELECTIONNES
!
    if (nbsel .gt. 0) then
        do 3 ilires = 1, nbmode
            do 33 ii = 0, nbsel
                if (ilires .eq. zi(idsel+ii)) goto 3
33          continue
!
            chaine = 'CBIDON'
            call codent(ilires, 'D0', chaine(1:5))
!
! TABLEAUX POUR LA PRESSION
!
            vestoc= '&&MAJOU.VESTOC'
            zk24(iadpr+ilires-1) = vestoc(1:14)//chaine(1:5)
            call wkvect(zk24(iadpr+ilires-1)(1:19)//'.VALE', 'V V R', nbvale, ivalp)
            call wkvect(zk24(iadpr+ilires-1)(1:19)//'.REFE', 'V V K24', nbrefe, irefp)
            call wkvect(zk24(iadpr+ilires-1)(1:19)//'.DESC', 'V V I', nbdesc, idesp)
!
! TABLEAUX POUR LES DEPLACEMENTS EN X
!
            vestoc= '&&MAJOU.TPXSTO'
            zk24(iadx+ilires-1) = vestoc(1:14)//chaine(1:5)
            call wkvect(zk24(iadx+ilires-1)(1:19)//'.VALE', 'V V R', nbvale, ivalp)
            call wkvect(zk24(iadx+ilires-1)(1:19)//'.REFE', 'V V K24', nbrefe, irefp)
            call wkvect(zk24(iadx+ilires-1)(1:19)//'.DESC', 'V V I', nbdesc, idesp)
!
! TABLEAUX POUR LES DEPLACEMENTS EN Y
!
            vestoc= '&&MAJOU.TPYSTO'
            zk24(iady+ilires-1) = vestoc(1:14)//chaine(1:5)
            call wkvect(zk24(iady+ilires-1)(1:19)//'.VALE', 'V V R', nbvale, ivalp)
            call wkvect(zk24(iady+ilires-1)(1:19)//'.REFE', 'V V K24', nbrefe, irefp)
            call wkvect(zk24(iady+ilires-1)(1:19)//'.DESC', 'V V I', nbdesc, idesp)
!
! TABLEAUX POUR LES DEPLACEMENTS EN Z
!
            if (model .eq. '3D') then
                vestoc= '&&MAJOU.TPZSTO'
                zk24(iadz+ilires-1) = vestoc(1:14)//chaine(1:5)
                call wkvect(zk24(iadz+ilires-1)(1:19)//'.VALE', 'V V R', nbvale, ivalp)
                call wkvect(zk24(iadz+ilires-1)(1:19)//'.REFE', 'V V K24', nbrefe, irefp)
                call wkvect(zk24(iadz+ilires-1)(1:19)//'.DESC', 'V V I', nbdesc, idesp)
            endif
!
 3      continue
    endif
!
! --- MENAGE
!
    call jedetr(tempor)
    call detrsd('CHAM_NO', vepr)
    call detrsd('CHAM_NO', chcomb)
    call detrsd('CHAM_NO', chcmb2)
    call detrsd('CHAM_NO', chflu)
    call detrsd('CHAM_NO', vesolx)
    call detrsd('CHAM_NO', vesoly)
    call detrsd('CHAM_NO', vesolz)
    call detrsd('CHAM_NO', chamnx)
    call detrsd('CHAM_NO', chamny)
    call detrsd('CHAM_NO', chamnz)
!
    call jedema()
end subroutine
