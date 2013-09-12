subroutine avgrno(vwork, tdisp, lisnoe, nbnot, nbordr,&
                  nnoini, nbnop, tspaq, nomcri, nomfor,&
                  grdvie, forvie, fordef, nommai, proaxe,&
                  nommap, cnsr, post, resu)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: van-xuan.tran at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/loisem.h"
#include "asterc/lor8em.h"
#include "asterc/r8pi.h"
#include "asterfort/anacri.h"
#include "asterfort/avplcr.h"
#include "asterfort/carces.h"
#include "asterfort/cncinv.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedisp.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/recofa.h"
#include "asterfort/rnomat.h"
#include "asterfort/u2mesg.h"
#include "asterfort/vecnuv.h"
#include "asterfort/wkvect.h"
!
    integer :: tdisp, nbnop, lisnoe(nbnop), nbnot, nbordr, nnoini
    integer :: tspaq
    logical :: fordef, post
    real(kind=8) :: vwork(tdisp), resu(7)
    character(len=8) :: nommai, grdvie, nommap
    character(len=16) :: nomcri, proaxe, nomfor, forvie
    character(len=19) :: cnsr
! ---------------------------------------------------------------------
! BUT: DETERMINER LE PLAN DANS LEQUEL LE DOMMAGE EST MAXIMAL
! ---------------------------------------------------------------------
! ARGUMENTS:
! VWORK     IN    R  : VECTEUR DE TRAVAIL CONTENANT
!                      L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                      ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                      DU <<PAQUET>> DE MAILLES.
! TDISP     IN    I  : DIMENSION DU VECTEUR VWORK
! LISNOE    IN    I  : LISTE COMPLETE DES NOEUDS A TRAITER.
! NBNOT     IN    I  : NOMBRE TOTAL DE NOEUDS A TRAITER.
! NBORDR    IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
!                      STRUCTURE DE DONNEES RESULTAT.
! NNOINI    IN    I  : NUMERO DU 1ER NOEUD DU <<PAQUET>> DE
!                      NOEUDS COURANT.
! NBNOP     IN    I  : NOMBRE DE NOEUDS DANS LE <<PAQUET>> DE
!                      NOEUDS COURANT.
! TSPAQ     IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                      COURANT.
! NOMCRI    IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
! NOMMAI    IN    K8 : NOM DU MAILLAGE.
! PROAXE    IN    K16: TYPE DE PROJECTION (UN OU DEUX AXES).
! CNSR      IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
!                      RESULTATS.
!
! REMARQUE :
!  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
!    MAILLES DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
!-----------------------------------------------------------------------
    integer :: i, ibid, jvectn, jvectu, jvectv
    integer :: jcnrd, jcnrl, jcnrv, iret, icesd, icesl, icesv
    integer :: tneces, tdisp2(1), jvecno, n, k
    integer :: nunoe, ideb, dim, j, ngam, tab2(18), ifin
    integer :: l, cnbno, ibidno, kwork, somnow, inop
    integer :: nbma, adrma, jtypma
    integer :: icmp, jad, iarg
    integer :: vali(2), nbvecm, paract(30)
!
    real(kind=8) :: fatsoc, dgam, gamma, pi, dphi, tab1(18), phi0
    real(kind=8) :: vala, valb, coefpa, cudomx
    real(kind=8) :: nxm(2), nym(2), nzm(2)
    real(kind=8) :: vresu(24)
!
    logical :: lbid, crsigm
!
    character(len=8) :: chmat1, nommat
    character(len=10) :: optio
    character(len=16) :: typcha
    character(len=19) :: chmat, cesmat, ncncin
    character(len=24) :: typma
!
!
!-----------------------------------------------------------------------
!234567                                                              012
!-----------------------------------------------------------------------
    data  tab1/ 180.0d0, 60.0d0, 30.0d0, 20.0d0, 15.0d0, 12.857d0,&
     &             11.25d0, 10.588d0, 10.0d0, 10.0d0, 10.0d0, 10.588d0,&
     &             11.25d0, 12.857d0, 15.0d0, 20.0d0, 30.0d0, 60.0d0 /
!
    data  tab2/ 1, 3, 6, 9, 12, 14, 16, 17, 18, 18, 18, 17, 16, 14,&
     &           12, 9, 6, 3 /
!
    pi = r8pi()
!-----------------------------------------------------------------------
!
    call jemarq()
!
! CONSTRUCTION DU VECTEUR NORMAL SUR UNE DEMI SPHERE
! CONSTRUCTION DU VECTEUR U DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
! CONSTRUCTION DU VECTEUR V DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
!
    call wkvect('&&AVGRNO.VECT_NORMA', 'V V R', 627, jvectn)
    call wkvect('&&AVGRNO.VECT_TANGU', 'V V R', 627, jvectu)
    call wkvect('&&AVGRNO.VECT_TANGV', 'V V R', 627, jvectv)
!
! OBTENTION DES ADRESSES '.CNSD', '.CNSL' ET '.CNSV' DU CHAMP SIMPLE
! DESTINE A RECEVOIR LES RESULTATS : DOMMAGE_MAX, COORDONNEES VECTEUR
! NORMAL CORRESPONDANT
!
    if (.not. post) then
        call jeveuo(cnsr//'.CNSD', 'L', jcnrd)
        call jeveuo(cnsr//'.CNSL', 'E', jcnrl)
        call jeveuo(cnsr//'.CNSV', 'E', jcnrv)
!
! RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR
!
        call getvid(' ', 'CHAM_MATER', scal=chmat1, nbret=iret)
        chmat = chmat1//'.CHAMP_MAT'
        cesmat = '&&AVGRNO.CESMAT'
        call carces(chmat, 'ELEM', ' ', 'V', cesmat,&
                    'A', iret)
        call jeveuo(cesmat//'.CESD', 'L', icesd)
        call jeveuo(cesmat//'.CESL', 'L', icesl)
        call jeveuo(cesmat//'.CESV', 'L', icesv)
!
    endif
!
! DEFINITION DU VECTEUR CONTENANT LES VALEURS DU CISAILLEMENT POUR TOUS
! LES INSTANTS ET TOUS LES PLANS
!
    tneces = 209*nbordr*2
    call jedisp(1, tdisp2)
    tdisp2(1) = (tdisp2(1) * loisem()) / lor8em()
    if (tdisp2(1) .lt. tneces) then
        vali (1) = tdisp2(1)
        vali (2) = tneces
        call u2mesg('F', 'PREPOST5_8', 0, ' ', 2,&
                    vali, 0, 0.d0)
    else
        call wkvect('&&AVGRNO.VECTNO', 'V V R', tneces, jvecno)
    endif
!
    typcha = 'NON_PERIODIQUE'
!
!    DECPRO POUR IDENTIFIER L'AXE A PRPJECTER
!---    ANALYSER LE CRITERE
    crsigm = .false.
    call anacri(nomcri, nomfor, typcha, 'NON', paract,&
                lbid, crsigm, lbid, lbid, lbid)
    fatsoc = 1.0d0
!
    if (( nomcri(1:16) .eq. 'FATESOCI_MODI_AV' ) .or. fordef .or. (.not. (crsigm))) then
        fatsoc = 1.0d4
    else
        fatsoc = 1.0d0
    endif
!
! CONSTRUCTION DES VECTEURS N, U ET V
!
    dgam = 10.0d0
!
    n = 0
    k = 1
    ideb = 1
    dim = 627
    do 300 j = 1, 18
        gamma=(j-1)*dgam*(pi/180.0d0)
        dphi=tab1(j)*(pi/180.0d0)
        ngam=tab2(j)
        ifin = ngam
        phi0 = dphi/2.0d0
!
        call vecnuv(ideb, ifin, gamma, phi0, dphi,&
                    n, k, dim, zr( jvectn), zr(jvectu),&
                    zr(jvectv))
!
300  end do
!
! CONSTRUCTION DU VECTEUR : CONTRAINTE = F(NUMERO D'ORDRE) EN CHAQUE
! NOEUDS DU PAQUET DE MAILLES.
    l = 1
    cnbno = 0
    kwork = 0
    somnow = 0
    ibidno = 1
!
    ncncin = '&&AVGRNO.CNCINV'
!
    if (.not. post) then
!
        call cncinv(nommai, ibid, 0, 'V', ncncin)
        typma = nommai//'.TYPMAIL'
        call jeveuo(typma, 'L', jtypma)
    endif
!
    do 400 inop = nnoini, nnoini+(nbnop-1)
!
        if (inop .gt. nnoini) then
            kwork = 1
            somnow = somnow + 1
        endif
!
        cnbno = cnbno + 1
        if ((l*int(nbnot/10.0d0)) .lt. cnbno) then
            l = l + 1
        endif
!
! RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE OU AUX MAILLES
! QUI PORTENT LE NOEUD COURANT.
        if (.not. post) then
!
            nunoe = lisnoe(inop)
            call jelira(jexnum(ncncin, nunoe), 'LONMAX', nbma)
            call jeveuo(jexnum(ncncin, nunoe), 'L', adrma)
!
            k = 0
            optio = 'DOMA_NOEUD'
!
!
            do 410, i=1, nbma
            call rnomat(icesd, icesl, icesv, i, nomcri,&
                        adrma, jtypma, k, optio, vala,&
                        valb, coefpa, nommat)
410          continue
!
!
!
            if (k .eq. 0) then
                vali (1) = nunoe
                vali (2) = nbma
                call u2mesg('A', 'PREPOST5_10', 0, ' ', 2,&
                            vali, 0, 0.d0)
            endif
!
        endif
!
!
        if (post) then
            nommat = nommap
!
! RECUPERER LES COEEF DE CRITERES
!
            call recofa(nomcri, nommat, vala, valb, coefpa)
!
        endif
!
        nbvecm = 209
!
! REMPLACER PAR AVPLCR
        call avplcr(nbvecm, zr(jvectn), zr(jvectu), zr(jvectv), nbordr,&
                    kwork, somnow, vwork, tdisp, tspaq,&
                    ibidno, nomcri, nomfor, grdvie, forvie,&
                    fordef, fatsoc, proaxe, nommat, vala,&
                    coefpa, post, cudomx, nxm, nym,&
                    nzm)
!
! 11. CONSTRUCTION D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
!     POUR CHAQUE POINT DE GAUSS DE CHAQUE MAILLE LE DOMMAGE_MAX ET LE
!     VECTEUR NORMAL ASSOCIE.
!
        do 600 icmp = 1, 24
            vresu(icmp) = 0.0d0
600      continue
!
        do 601 icmp = 1, 7
            resu(icmp) = 0.0d0
601      continue
!
        vresu(2) = nxm(1)
        vresu(3) = nym(1)
        vresu(4) = nzm(1)
        vresu(11) = cudomx
        vresu(13) = nxm(2)
        vresu(14) = nym(2)
        vresu(15) = nzm(2)
!
! 12. AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE
        if (post) then
            resu(1) = nxm(1)
            resu(2) = nym(1)
            resu(3) = nzm(1)
            resu(4) = cudomx
            resu(5) = nxm(2)
            resu(6) = nym(2)
            resu(7) = nzm(2)
            goto 400
        else
!
            do 610 icmp = 1, 24
                jad = 24*(nunoe-1) + icmp
                zl(jcnrl - 1 + jad) = .true.
                zr(jcnrv - 1 + jad) = vresu(icmp)
610          continue
!
        endif
!
400  end do
!
! MENAGE
!
    if (.not. post) then
        call detrsd('CHAM_ELEM_S', cesmat)
    endif
!
    call jedetr('&&AVGRNO.VECT_NORMA')
    call jedetr('&&AVGRNO.VECT_TANGU')
    call jedetr('&&AVGRNO.VECT_TANGV')
!
    call jedema()
end subroutine
