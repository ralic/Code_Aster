subroutine elref6(elrz, nomtz, famiz, ndim, nno,&
                  nnos, npg, ipoids, jcoopg, ivf,&
                  idfde, jdfd2, jgano)
    implicit none
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/elraca.h"
#include "asterfort/indk32.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: elrz, nomtz, famiz
    integer :: ndim, nno, nnos, npg, ipoids, jcoopg, ivf, idfde, jdfd2, jgano
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
! ----------------------------------------------------------------------
! BUT: RECUPERER DANS UNE ROUTINE LES ADRESSES DANS ZR
!      - DES POIDS DES POINTS DE GAUSS  : IPOIDS
!      - DES COORDONNEES DES POINTS DE GAUSS  : JCOOPG
!      - DES VALEURS DES FONCTIONS DE FORME : IVF
!      - DES VALEURS DES DERIVEES 1ERES DES FONCTIONS DE FORME : IDFDE
!      - DES VALEURS DES DERIVEES 2EMES DES FONCTIONS DE FORME : JDFD2
!      - DE LA MATRICE DE PASSAGE GAUSS -> NOEUDS : JGANO
! ----------------------------------------------------------------------
!   IN   ELRZ  : NOM DE L'ELREFA (K8)
!        NOMTE : NOM DU TYPE D'ELEMENT (K16)
!        FAMIL  : NOM (LOCAL) DE LA FAMILLE DE POINTS DE GAUSS :
!                 'STD','RICH',...
!   OUT  NDIM   : DIMENSION DE L'ESPACE (=NB COORDONNEES)
!        NNO    : NOMBRE DE NOEUDS DU TYPE_MAILLE
!        NNOS   : NOMBRE DE NOEUDS SOMMETS DU TYPE_MAILLE
!        NPG    : NOMBRE DE POINTS DE GAUSS
!        IPOIDS : ADRESSE DANS ZR DU TABLEAU POIDS(IPG)
!        JCOOPG : ADRESSE DANS ZR DU TABLEAU COOPG(IDIM,IPG)
!        IVF    : ADRESSE DANS ZR DU TABLEAU FF(INO,IPG)
!        IDFDE  : ADRESSE DANS ZR DU TABLEAU DFF(IDIM,INO,IPG)
!        JDFD2  : ADRESSE DANS ZR DU TABLEAU DFF2(IDIM,JDIM,INO,IPG)
!        JGANO  : ADRESSE DANS ZR DE LA MATRICE DE PASSAGE
!                      GAUSS -> NOEUDS (DIM= 2+NNO*NPG)
!                 ATTENTION : LES 2 1ERS TERMES SONT LES
!                             DIMMENSIONS DE LA MATRICE: NNO ET NPG
!
!   -------------------------------------------------------------------
    character(len=16) :: nomte
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
!
    integer :: nbnomx, nbfamx
    parameter    ( nbnomx=27, nbfamx=20)
    character(len=8) :: elrf, famil, fapg(nbfamx)
    character(len=16) :: nofgpg
    character(len=32) :: noflpg
    integer :: nbfpg, nbpg(nbfamx), jvr, decal, ifam, lonfam
    integer :: nufpg, nufgpg, nuflpg, jdfd2l, jganol
    integer :: ndiml, nnosl, nnol, npgl, ipoidl, jcoopl, ivfl, idfdel
    real(kind=8) :: vol, x(3*nbnomx)
!
!     -- POUR FAIRE DES "SAVE" ET GAGNER EN PERFORMANCE :
    integer :: maxsav
    parameter (maxsav=5)
    integer :: nbsav
    common /caii13/nbsav
    integer :: addsav(5, 10), k1, k2, nusav
    character(len=32) :: nomsav(maxsav)
    save nomsav,addsav
! DEB ------------------------------------------------------------------
!
!     POUR ETRE SUR QUE ELREF6 EST APPELE SOUS CALCUL
!
    ASSERT(iactif.eq.1)
!
    famil = famiz
    elrf = elrz
    nomte = nomtz
    noflpg = nomte//elrf//famil
!
!     -- POUR GAGNER DU TEMPS, ON REGARDE SI LA FAMILLE A ETE SAUVEE:
!     ---------------------------------------------------------------
    nusav = indk32(nomsav,noflpg,1,nbsav)
    if (nusav .gt. 0) then
        ndiml = addsav(nusav,1)
        nnol = addsav(nusav,2)
        nnosl = addsav(nusav,3)
        npgl = addsav(nusav,4)
        ipoidl = addsav(nusav,5)
        jcoopl = addsav(nusav,6)
        ivfl = addsav(nusav,7)
        idfdel = addsav(nusav,8)
        jdfd2l = addsav(nusav,9)
        jganol = addsav(nusav,10)
        goto 40
    endif
!
!
!     -- CALCUL DE NUFPG :
!     --------------------
    nuflpg = indk32(zk32(jpnlfp),noflpg,1,nblfpg)
    if (nuflpg .eq. 0) call u2mesk('F', 'DVP_5', 1, noflpg)
    nufgpg = zi(jnolfp-1+nuflpg)
    if (nufgpg .eq. 0) call u2mesk('F', 'CALCULEL2_45', 1, noflpg)
    call jenuno(jexnum('&CATA.TM.NOFPG', nufgpg), nofgpg)
    ASSERT(nofgpg(1:8).eq.elrf)
    call elraca(elrf, ndiml, nnol, nnosl, nbfpg,&
                fapg, nbpg, x, vol)
    ASSERT(nbfpg.lt.nbfamx)
    nufpg = indik8(fapg,nofgpg(9:16),1,nbfpg)
    ASSERT(nufpg.gt.0)
!
!
    call jeveuo('&INEL.'//elrf//'.ELRA_R', 'L', jvr)
!
    decal = 0
    do 10,ifam = 1,nufpg - 1
    npgl = nbpg(ifam)
!
    lonfam = npgl
    lonfam = lonfam + npgl*ndiml
    lonfam = lonfam + npgl*nnol
    lonfam = lonfam + npgl*nnol*ndiml
    lonfam = lonfam + npgl*nnol*ndiml*ndiml
    lonfam = lonfam + 2 + npgl*nnol
!
    decal = decal + lonfam
    10 end do
!
    npgl = nbpg(nufpg)
!
    ipoidl = jvr + decal
    jcoopl = ipoidl + npgl
    ivfl = jcoopl + npgl*ndiml
    idfdel = ivfl + npgl*nnol
    jdfd2l = idfdel + npgl*nnol*ndiml
    jganol = jdfd2l + npgl*nnol*ndiml*ndiml
!
!
!     -- ON SAUVEGARDE LES VALEURS CALCULEES :
!     ----------------------------------------
!     -- ON DECALE TOUT LE MONDE VERS LE BAS:
    nbsav = min(nbsav+1,maxsav)
    do 30,k1 = nbsav - 1,1,-1
    nomsav(k1+1) = nomsav(k1)
    do 20,k2 = 1,10
    addsav(k1+1,k2) = addsav(k1,k2)
20  continue
    30 end do
!
!     -- ON RECOPIE LES NOUVELLES VALEURS EN POSITION 1 :
    nomsav(1) = noflpg
    addsav(1,1) = ndiml
    addsav(1,2) = nnol
    addsav(1,3) = nnosl
    addsav(1,4) = npgl
    addsav(1,5) = ipoidl
    addsav(1,6) = jcoopl
    addsav(1,7) = ivfl
    addsav(1,8) = idfdel
    addsav(1,9) = jdfd2l
    addsav(1,10) = jganol
!
40  continue
!
    ndim = ndiml
    nnos = nnosl
    nno = nnol
    npg = npgl
    ipoids = ipoidl
    jcoopg = jcoopl
    ivf = ivfl
    idfde = idfdel
    jdfd2 = jdfd2l
    jgano = jganol
!
end subroutine
