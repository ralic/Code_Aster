subroutine dtauno(jrwork, lisnoe, nbnot, nbordr, ordini,&
                  nnoini, nbnop, tspaq, nommet, nomcri,&
                  nomfor, grdvie, forvie, nommai, cnsr,&
                  nommap, post, valpar, vresu)
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
! person_in_charge: van-xuan.tran at edf.fr
    implicit none
#include "jeveux.h"
!
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/loisem.h"
#include "asterc/lor8em.h"
#include "asterc/r8pi.h"
#include "asterfort/acgrdo.h"
#include "asterfort/carces.h"
#include "asterfort/cncinv.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedisp.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rcpare.h"
#include "asterfort/recofa.h"
#include "asterfort/rnomat.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vecnuv.h"
#include "asterfort/wkvect.h"
    integer :: jrwork, nbnot, lisnoe(nbnot), nbordr, nnoini, nbnop
    integer :: tspaq, ordini
    logical :: post
    real(kind=8) :: vresu(24), valpar(22)
    character(len=8) :: nommai, grdvie, nommap
    character(len=16) :: nomcri, nommet, nomfor, forvie
    character(len=19) :: cnsr
! ---------------------------------------------------------------------
! BUT: DETERMINER LE PLAN INCLINE POUR LEQUEL DELTA_TAU EST MAXIMUM
!      POUR CHAQUE NOEUD D'UN <<PAQUET>> DE NOEUDS.
! ---------------------------------------------------------------------
! ARGUMENTS:
! JRWORK     IN    I  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                       L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                       ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                       DU <<PAQUET>> DE MAILLES.
! LISNOE     IN    I  : LISTE COMPLETE DES NOEUDS A TRAITER.
! NBNOT      IN    I  : NOMBRE TOTAL DE NOEUDS A TRAITER.
! NBORDR     IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
!                       STRUCTURE DE DONNEES RESULTAT.
! ORDINI     IN    I  : ORDRE INITIAL POUR LE CHARGEMENT CYCLIQUE
! NNOINI     IN    I  : NUMERO DU 1ER NOEUD DU <<PAQUET>> DE
!                       NOEUDS COURANT.
! NBNOP      IN    I  : NOMBRE DE NOEUDS DANS LE <<PAQUET>> DE
!                       NOEUDS COURANT.
! TSPAQ      IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE NOEUDS
!                       COURANT.
! NOMMET     IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
!                       CIRCONSCRIT.
! NOMCRI     IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
! NOMMAI     IN    K8 : NOM UTILISATEUR DU MAILLAGE.
! CNSR       IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
!                       RESULTATS.
!
! REMARQUE :
!  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
!    NOEUDS DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
!-----------------------------------------------------------------------
!
    integer :: j, k, ki, l, n, jcnrd, jcnrl, jcnrv, ibidno
    integer :: iret, nbma, adrma, icesd, icesl, icesv
    integer :: ibid, tneces, tdisp(1), jvecno
    integer :: jvectn, jvectu, jvectv, ngam, ideb, dim
    integer :: tab2(18), inop, nunoe, jdtaum, jresun
    integer :: jtypma
    integer :: icmp, kwork, somnow, cnbno
    integer :: vali(2), jad, iarg, ima
!
    real(kind=8) :: dgam, pi, dphi, tab1(18)
    real(kind=8) :: phi0, coepre, vala, valb, gamma
    real(kind=8) :: coefpa
!
    integer :: icodwo
    character(len=8) :: chmat1, nommat
    character(len=10) :: optio
    character(len=19) :: chmat, cesmat, ncncin
    character(len=24) :: typma
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
! CONSTRUCTION DU VECTEUR CONTENANT DELTA_TAU_MAX
! CONSTRUCTION DU VECTEUR CONTENANT LA VALEUR DU POINTEUR PERMETTANT
!              DE RETROUVER LE VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX
!
    call wkvect('&&DTAUNO.DTAU_MAX', 'V V R', 209, jdtaum)
    call wkvect('&&DTAUNO.RESU_N', 'V V I', 209, jresun)
!
! CONSTRUCTION DU VECTEUR NORMAL SUR UNE DEMI SPHERE
! CONSTRUCTION DU VECTEUR U DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
! CONSTRUCTION DU VECTEUR V DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
!
    call wkvect('&&DTAUNO.VECT_NORMA', 'V V R', 630, jvectn)
    call wkvect('&&DTAUNO.VECT_TANGU', 'V V R', 630, jvectu)
    call wkvect('&&DTAUNO.VECT_TANGV', 'V V R', 630, jvectv)
!
! OBTENTION DES ADRESSES '.CESD', '.CESL' ET '.CESV' DU CHAMP SIMPLE
! DESTINE A RECEVOIR LES RESULTATS : DTAUM, ....
    if (.not. post) then
        call jeveuo(cnsr//'.CNSD', 'L', jcnrd)
        call jeveuo(cnsr//'.CNSL', 'E', jcnrl)
        call jeveuo(cnsr//'.CNSV', 'E', jcnrv)
!
! RECUPERATION DU COEFFICIENT DE PRE-ECROUISSAGE DONNE PAR L'UTILISATEUR
!
        call getvr8(' ', 'COEF_PREECROU', 1, iarg, 1,&
                    coepre, iret)
!
! RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR
!
        call getvid(' ', 'CHAM_MATER', 1, iarg, 1,&
                    chmat1, iret)
        chmat = chmat1//'.CHAMP_MAT'
        cesmat = '&&DTAUNO.CESMAT'
        call carces(chmat, 'ELEM', ' ', 'V', cesmat,&
                    'A', iret)
        call jeveuo(cesmat//'.CESD', 'L', icesd)
        call jeveuo(cesmat//'.CESL', 'L', icesl)
        call jeveuo(cesmat//'.CESV', 'L', icesv)
!
    endif
!
    tneces = 209*nbordr*2
    call jedisp(1, tdisp)
    tdisp(1) = (tdisp(1) * loisem()) / lor8em()
    if (tdisp(1) .lt. tneces) then
        vali (1) = tdisp(1)
        vali (2) = tneces
        call u2mesg('F', 'PREPOST5_8', 0, ' ', 2,&
                    vali, 0, 0.d0)
    else
        call wkvect('&&DTAUNO.VECTNO', 'V V R', tneces, jvecno)
        call jerazo('&&DTAUNO.VECTNO', tneces, 1)
    endif
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
        phi0=dphi/2.0d0
        ngam=tab2(j)
!
        call vecnuv(ideb, ngam, gamma, phi0, dphi,&
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
!
    ncncin = '&&DTAUNO.CNCINV'
!
    if (.not. post) then
!
        call cncinv(nommai, ibid, 0, 'V', ncncin)
        typma = nommai//'.TYPMAIL'
        call jeveuo(typma, 'L', jtypma)
!
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
            nunoe = lisnoe(inop)
            call jelira(jexnum(ncncin, nunoe), 'LONMAX', nbma)
            call jeveuo(jexnum(ncncin, nunoe), 'L', adrma)
!
            ki = 0
            optio = 'DOMA_NOEUD'
            do 410, ima=1, nbma
            call rnomat(icesd, icesl, icesv, ima, nomcri,&
                        adrma, jtypma, ki, optio, vala,&
                        valb, coefpa, nommat)
410          continue
!
            call rcpare(nommat, 'FATIGUE', 'WOHLER', icodwo)
            if (icodwo .eq. 1) then
                call u2mesk('F', 'FATIGUE1_90', 1, nomcri(1:16))
            endif
!
            if (k .eq. 0) then
                vali (1) = nunoe
                vali (2) = nbma
                call u2mesg('A', 'PREPOST5_10', 0, ' ', 2,&
                            vali, 0, 0.d0)
            endif
        endif
!
        call jerazo('&&DTAUNO.VECTNO', tneces, 1)
!
! C  IBIDNO JOUE LE ROLE DE IPG DNAS TAURLO
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
        ibidno = 1
!
!
! REMPACER PAR ACMATA
        call acgrdo(jvectn, jvectu, jvectv, nbordr, ordini,&
                    kwork, somnow, jrwork, tspaq, ibidno,&
                    jvecno, jdtaum, jresun, nommet, nommat,&
                    nomcri, vala, coefpa, nomfor, grdvie,&
                    forvie, valpar, vresu)
!
! AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE
        if (.not. post) then
            do 550 icmp = 1, 24
                jad = 24*(nunoe-1) + icmp
                zl(jcnrl - 1 + jad) = .true.
                zr(jcnrv - 1 + jad) = vresu(icmp)
!
550          continue
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
    call jedetr('&&DTAUNO.DTAU_MAX')
    call jedetr('&&DTAUNO.RESU_N')
    call jedetr('&&DTAUNO.VECT_NORMA')
    call jedetr('&&DTAUNO.VECT_TANGU')
    call jedetr('&&DTAUNO.VECT_TANGV')
    call jedetr('&&DTAUNO.VECTNO')
    call jedetr('&&DTAUNO.CNCINV')
!
    call jedema()
end subroutine
