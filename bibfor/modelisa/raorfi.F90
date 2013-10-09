subroutine raorfi(noma, ligrel, noepou, cara, coorig,&
                  eg1, eg2, eg3, typrac, rayon)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/carcou.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/infniv.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mecact.h"
#include "asterfort/normev.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vdiff.h"
!
    integer :: info, ifm
    character(len=8) :: noepou, noma, cara
    character(len=19) :: ligrel
    real(kind=8) :: coorig(3), eg1(3), eg2(3), eg3(3), r
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
! -------------------------------------------------------
!     POUR LE RACCORD (COQUE OU 3D)_TUYAU
!
!
    integer :: nma, ima, inopou, iconex, nbno, ier, i, j, ntseg3, jdtm, nutyma
    integer :: idesc, ncmpmx, ivale, iptma, igd, idebgd, jcoor
    integer :: inopo1, inopo2, icoude, nno, ntseg4
    character(len=8) ::  nomgd, nocmp(3), noepo1, noepo2, typrac
    character(len=19) :: chcara
    real(kind=8) :: coorif(3), gpl(3), gpg(3), pgl(3, 3)
    real(kind=8) :: el1(3), el2(3), el3(3), coono1(3), coono2(3), e1(3), nore1
    real(kind=8) :: rayon
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), l, omega, theta
    real(kind=8) :: pgl4(3, 3)
!
    call jemarq()
    call infniv(ifm, info)
!
!     RECHERCHE DE LA MAILLE IMA  CONTENANT LE NOEUD NOEPOU
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nma)
    ima = 0
    call jenonu(jexnom(noma//'.NOMNOE', noepou), inopou)
    do 55 i = 1, nma
        call jeveuo(jexnum(noma//'.CONNEX', i), 'L', iconex)
        call jelira(jexnum(noma//'.CONNEX', i), 'LONMAX', nbno)
        do 56 j = 1, nbno
            if (zi(iconex+j-1) .eq. inopou) then
                if (ima .eq. 0) then
                    ima = i
                    inopo1=zi(iconex)
                    inopo2=zi(iconex+1)
                else
                    call utmess('F', 'MODELISA6_36', sk=noepou)
                endif
            endif
 56     continue
 55 end do
!
!     VERIFICATION QUE LA MAILLE IMA EST UN SEG3
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG3'), ntseg3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG4'), ntseg4)
    call jeveuo(noma//'.TYPMAIL', 'L', jdtm)
    nutyma = zi(jdtm+ima-1)
    if (nutyma .eq. ntseg3) then
        nno=3
    else if (nutyma.eq.ntseg4) then
        nno=4
    else
        call utmess('F', 'MODELISA6_37', sk=noepou)
    endif
!
!     RECUPERATION DES ANGLES NAUTIQUES DANS LA CARTE ORIENTATION
!
    chcara = cara(1:8)//'.CARORIEN'
    call etenca(chcara, ligrel, ier)
    ASSERT(ier.eq.0)
    nomgd = 'CAORIE'
    call jeveuo(chcara//'.DESC', 'L', idesc)
!
! --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR CF NUROTA
!
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx)
!
! --- TABLEAU DE VALEURS DE LA CARTE CHCARA
!
    call jeveuo(chcara//'.VALE', 'L', ivale)
!
! --- RECUPERATION DU VECTEUR D'ADRESSAGE DANS LA CARTE CREE PAR ETENCA
!
    call jeveuo(chcara//'.PTMA', 'L', iptma)
!
!     RECUPERATION DES ANGLES NAUTIQUES
!
    if (zi(iptma+ima-1) .ne. 0) then
        igd = zi(iptma+ima-1)
        idebgd = (igd-1)*ncmpmx
!        RECUPERATION DE L'ORIENTATION
!         DO 10 I=1,3
!            ORIEN(I) = ZR(IVALE+IDEBGD+I-1)
!10       CONTINUE
        call carcou(zr(ivale+idebgd), l, pgl, r, theta,&
                    pgl1, pgl2, pgl3, pgl4, nno,&
                    omega, icoude)
    else
        call utmess('F', 'MODELISA6_38')
    endif
!
!     CALCUL DU VECTEUR E1 ORIENTANT LA MAILLE TUYAU
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    coono1(1) = zr(jcoor-1+3*(inopo1-1)+1)
    coono1(2) = zr(jcoor-1+3*(inopo1-1)+2)
    coono1(3) = zr(jcoor-1+3*(inopo1-1)+3)
    coono2(1) = zr(jcoor-1+3*(inopo2-1)+1)
    coono2(2) = zr(jcoor-1+3*(inopo2-1)+2)
    coono2(3) = zr(jcoor-1+3*(inopo2-1)+3)
    call vdiff(3, coono2, coono1, e1)
    call normev(e1, nore1)
    nocmp(1) = 'X'
    nocmp(2) = 'Y'
    nocmp(3) = 'Z'
!
    call mecact('V', typrac//'.CAXE_TUY', 'LIGREL', ligrel, 'GEOM_R',&
                ncmp=3, lnomcmp=nocmp, vr=e1)
!
!     CALCUL DU VECTEUR GPL, AVEC P ORIGINE DE PHI
!
    gpl(1)=0.d0
    gpl(2)=0.d0
!1      GPL(3)=RAYON FORTRAN ACTUEL
!2      GPL(3)=-RAYON  LOGIQUE SUIVANT DOC R
    gpl(3)=-rayon
!
!     PASSAGE DE GPL DANS LE REPERE GLOBAL ET COORDONNEES DE P
!
!      CALL MATROT (ORIEN,PGL)
    call utpvlg(1, 3, pgl, gpl, gpg)
    coorif(1) = gpg(1) + coorig(1)
    coorif(2) = gpg(2) + coorig(2)
    coorif(3) = gpg(3) + coorig(3)
!
! --- NOTATION DANS LA CARTE DE NOM '&&RAPOCO.CAORIFI' DES
! --- COORDONNEES DU POINT ORGINE DE PHI SUR LA SECTION DE RACCORD
!
    nocmp(1) = 'X'
    nocmp(2) = 'Y'
    nocmp(3) = 'Z'
!
    call mecact('V', typrac//'.CAORIFI', 'LIGREL', ligrel, 'GEOM_R',&
                ncmp=3, lnomcmp=nocmp, vr=coorif)
!
!     COORDONNEES DES VECTEURS UNITAIRES DANS LE REPERE GLOBAL
!
    el1(1)=1.d0
    el1(2)=0.d0
    el1(3)=0.d0
!
    el2(1)=0.d0
    el2(2)=1.d0
    el2(3)=0.d0
!
!        A CAUSE DE LA DEFINITION DU REPERE LOCAL, OU Z EST OPPOSE A
!        CELUI OBTENU PAR ROTATION DE ALPHA, BETA, GAMMA, IL FAUT
!        MODIFIER LE SIGNE DE Z (VERIF FAITE SUR LA FLEXION HORS PLAN)
!
    el3(1)=0.d0
    el3(2)=0.d0
!      EL3(3)=-1.D0 NON DIRECT  MAIS FTN ACTUEL
!      EL3(3)=1.D0   SERAIT LOGIQUE
    el3(3)=1.d0
!
    call utpvlg(1, 3, pgl, el1, eg1)
    call utpvlg(1, 3, pgl, el2, eg2)
    call utpvlg(1, 3, pgl, el3, eg3)
!
    if (info .eq. 2) then
        call jenuno(jexnum(noma//'.NOMNOE', inopo1), noepo1)
        call jenuno(jexnum(noma//'.NOMNOE', inopo2), noepo2)
        ifm = iunifi('MESSAGE')
        write(ifm,*) 'RAYON DE LA SECTION COQUE OU 3D ',rayon
        write(ifm,*) 'BARYCENTRE DE LA SECTION COQUE OU 3D ',coorig
        write(ifm,*) 'POINT ORIGINE DE LA GENERATRICE ',coorif
        write(ifm,*) 'VECTEUR AXE DU TUYAU : E1 ',e1
        write(ifm,*) 'NOEUDS AXE DU TUYAU :  ',noepo1,noepo2
        write(ifm,*) 'VECTEURS UNITAIRES DU TUYAU : E1 ',eg1
        write(ifm,*) 'VECTEURS UNITAIRES DU TUYAU : E2 ',eg2
        write(ifm,*) 'VECTEURS UNITAIRES DU TUYAU : E3 ',eg3
    endif
!
    call jedetr(chcara//'.PTMA')
    call jedema()
end subroutine
