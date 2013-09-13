subroutine avgrma(vwork, tdisp, vnbpg, nbpgt, nbordr,&
                  nmaini, nbmap, numpaq, tspaq, nomcri,&
                  nomfor, grdvie, forvie, fordef, proaxe,&
                  cesr)
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
#include "asterfort/assert.h"
#include "asterfort/avplcr.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedisp.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rnomat.h"
#include "asterfort/u2mesg.h"
#include "asterfort/vecnuv.h"
#include "asterfort/wkvect.h"
    integer :: tdisp, nbmap, vnbpg(nbmap), nbpgt, nbordr, nmaini
    integer :: numpaq, tspaq
    real(kind=8) :: vwork(tdisp)
    character(len=8) :: grdvie
    character(len=16) :: nomcri, proaxe, nomfor, forvie
    character(len=19) :: cesr
    logical :: fordef, post
! ---------------------------------------------------------------------
! BUT: DETERMINER LE PLAN DANS LEQUEL LE DOMMAGE EST MAXIMAL
! ---------------------------------------------------------------------
! ARGUMENTS:
! VWORK     IN    R  : VECTEUR DE TRAVAIL CONTENANT
!                      L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                      ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                      DU <<PAQUET>> DE MAILLES.
! TDISP     IN    I  : DIMENSION DU VECTEUR VWORK
! VNBPG     IN    I  : VECTEUR CONTENANT LE NOMBRE DE
!                      POINT DE GAUSS DE CHAQUE MAILLE DU MAILLAGE.
! NBPGT     IN    I  : NOMBRE TOTAL DE POINTS DE GAUSS A TRAITER.
! NBORDR    IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
!                      STRUCTURE DE DONNEES RESULTAT.
! NMAINI    IN    I  : NUMERO DE LA 1ERE MAILLE DU <<PAQUET>> DE
!                      MAILLES COURANT.
! NBMAP     IN    I  : NOMBRE DE MAILLES DANS LE <<PAQUET>> DE
!                      MAILLES COURANT.
! NUMPAQ    IN    I  : NUMERO DU PAQUET DE MAILLES COURANT.
! TSPAQ     IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                      COURANT.
! NOMCRI    IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
! PROAXE    IN    K16: TYPE DE PROJECTION (UN OU DEUX AXES).
! CESR      IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
!                      RESULTATS.
!
! REMARQUE :
!  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
!    MAILLES DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jvectn, jvectu, jvectv, nbvecm
    integer :: jcerd, jcerl, jcerv, iret, icesd, icesl, icesv, ibid
    integer :: tneces, tdisp2(1), jvecpg, n, k
    integer :: ideb, dim, j, ngam, tab2(18), ifin
    integer :: l, nbpg, nbpgp, kwork, sompgw, imap
    integer :: ipg
    integer :: icmp, jad
    integer :: vali(2)
!
    real(kind=8) :: fatsoc, dgam, gamma, pi, dphi, tab1(18), phi0
    real(kind=8) :: vala, valb, coefpa, cudomx
    real(kind=8) :: nxm(2), nym(2), nzm(2)
    real(kind=8) :: vresu(24)
!
    character(len=8) :: chmat1, nommat
    character(len=10) :: optio
    character(len=19) :: chmat, cesmat
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
    call wkvect('&&AVGRMA.VECT_NORMA', 'V V R', 627, jvectn)
    call wkvect('&&AVGRMA.VECT_TANGU', 'V V R', 627, jvectu)
    call wkvect('&&AVGRMA.VECT_TANGV', 'V V R', 627, jvectv)
!
!
! OBTENTION DES ADRESSES '.CESD', '.CESL' ET '.CESV' DU CHAMP SIMPLE
! DESTINE A RECEVOIR LES RESULTATS : DOMMAGE_MAX, COORDONNEES VECTEUR
! NORMAL CORRESPONDANT
!
    call jeveuo(cesr//'.CESD', 'L', jcerd)
    call jeveuo(cesr//'.CESL', 'E', jcerl)
    call jeveuo(cesr//'.CESV', 'E', jcerv)
!
! RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR
!
    call getvid(' ', 'CHAM_MATER', scal=chmat1, nbret=iret)
    chmat = chmat1//'.CHAMP_MAT'
    cesmat = '&&AVGRMA.CESMAT'
    call carces(chmat, 'ELEM', ' ', 'V', cesmat,&
                'A', iret)
    call jeveuo(cesmat//'.CESD', 'L', icesd)
    call jeveuo(cesmat//'.CESL', 'L', icesl)
    call jeveuo(cesmat//'.CESV', 'L', icesv)
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
        call wkvect('&&AVGRMA.VECTPG', 'V V R', tneces, jvecpg)
    endif
!
! COEFFICIENT PERMETTANT D'UTILISER LES MEMES ROUTINES POUR LES
! CONTRAINTES ET LES DEFORMATIONS
!
    if (( nomcri(1:16) .eq. 'FATESOCI_MODI_AV' ) .or. fordef) then
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
! CONSTRUCTION DU VECTEUR : CISAILLEMENT = F(NUMERO D'ORDRE) EN CHAQUE
! POINT DE GAUSS DU PAQUET DE MAILLES.
    l = 1
    nbpg = 0
    nbpgp = 0
    kwork = 0
    sompgw = 0
!
    do 400 imap = nmaini, nmaini+(nbmap-1)
        if (imap .gt. nmaini) then
            kwork = 1
            sompgw = sompgw + vnbpg(imap-1)
        endif
        nbpg = vnbpg(imap)
! SI LA MAILLE COURANTE N'A PAS DE POINTS DE GAUSS, LE PROGRAMME
! PASSE DIRECTEMENT A LA MAILLE SUIVANTE.
        if (nbpg .eq. 0) then
            goto 400
        endif
!
        nbpgp = nbpgp + nbpg
        if ((l*int(nbpgt/10.0d0)) .lt. nbpgp) then
            write(6,*)numpaq,'   ',(nbpgp-nbpg)
            l = l + 1
        endif
!
! RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE COURANTE
! ET DES PARAMETRES ASSOCIES AU CRITERE CHOISI POUR LA MAILLE COURANTE.
!
        optio = 'DOMA_ELGA'
        call rnomat(icesd, icesl, icesv, imap, nomcri,&
                    ibid, ibid, ibid, optio, vala,&
                    valb, coefpa, nommat)
!
!
!
        nbvecm = 209
!
        post = .false.
!
        do 420 ipg = 1, nbpg
!
!
! REMPLACER PAR AVPLCR
            call avplcr(nbvecm, zr(jvectn), zr(jvectu), zr(jvectv), nbordr,&
                        kwork, sompgw, vwork, tdisp, tspaq,&
                        ipg, nomcri, nomfor, grdvie, forvie,&
                        fordef, fatsoc, proaxe, nommat, vala,&
                        coefpa, post, cudomx, nxm, nym,&
                        nzm)
!
! RECUPERER LES RESULTATS
            do 600 icmp = 1, 24
                vresu(icmp) = 0.0d0
600          continue
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
!
            do 610 icmp = 1, 24
                call cesexi('C', jcerd, jcerl, imap, ipg,&
                            1, icmp, jad)
!
                ASSERT(jad .ne. 0)
                jad = abs(jad)
                zl(jcerl - 1 + jad) = .true.
                zr(jcerv - 1 + jad) = vresu(icmp)
!
610          continue
!
420      continue
400  end do
!
! MENAGE
!
    call detrsd('CHAM_ELEM_S', cesmat)
!
    call jedetr('&&AVGRMA.VECT_NORMA')
    call jedetr('&&AVGRMA.VECT_TANGU')
    call jedetr('&&AVGRMA.VECT_TANGV')
!
    call jedema()
end subroutine
