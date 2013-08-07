subroutine fonno6(resu, noma, ndim, ina, nbnose,&
                  iseg, nseg, noe, indr, nbnoel,&
                  vnor, vdir, basseg, vect, sens)
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
!
#include "asterc/getvtx.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/trigom.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
#include "blas/ddot.h"
    character(len=8) :: resu, noma
    integer :: ndim, ina, nbnose, iseg, noe(4, 4)
    integer :: indr(2), nbnoel, nseg
    real(kind=8) :: vdir(2, 3), vnor(2, 3), vect(3), sens
    character(len=19) :: basseg
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
!     ----------------------------------------------------------------
!     BUTS :
!        - VERIFIER LA COHERENCE DES 2 VECTEURS DIRECTION
!        - DANS LE CAS SYME : DETERMINER LA BONNE DIRECTION
!
!     ----------------------------------------------------
!
!  ENTREES
!       RESU   : NOM DU CONCEPT RESULTAT
!       NOMA   : NOM DU MAILLAGE
!       NDIM   : DIMENSION DU MODELE
!       INA    : INDICE DU NOEUD SOMMET DANS LA LISTE DES NOEUDS DU
!                FOND DE FISSURE
!       NBNOSE : NOMBRE DE NOEUD PAR SEGMENT
!       ISEG   : INDICE DU SEGMENT DU FOND DE FISSURE COURANT
!       NSEG   : NOMBRES DE SEGMENTS DU FOND DE FISSURE
!       NOE    : NOEUDS DES FACES CONTENANT NA et NB ET APPARTENANT AUX
!                MAILLES CONNECTEES AU NOEUD SOMMET COURANT
!                ET AUX LEVRES
!       INDR   : INDICES DES FACES LIBRES DANS LA LISTE DES FACES
!                DES MAILLES CONNECTEES AU FOND DE FISSURE
!       NBNOEL : NOMBRE DE NOEUDS SOMMETS PAR ELEMENTS
!       VNOR   : VECTEUR NORMAL A LA SURFACE DE LA FISSURE
!       VDIR   : VECTEUR DANS LA DIRECTION DE PROPAGATION
!  ENTREE/SORTIE
!       BAFEFO : BASES LOCALES PAR SEGMENT DU FOND (NORMEE)
!
!       ----------------------------------------------------
!
    integer :: jmale, iamase, ityp, iatyma, jtano, jtane, jbasse
    integer :: i, j, iret, inp, compt, ino, ifl
    integer :: ilev, itano, itane
    integer :: nblev, nn
    real(kind=8) :: s, ndir, nnor, alpha, angmax, beta
    real(kind=8) :: vecdir(ndim), vecnor(ndim), vnprec(ndim)
    real(kind=8) :: p, prvd1, prvd2
    character(len=6) :: syme
    character(len=8) :: k8b, type
    parameter    (angmax=2.5d0)
    integer :: iarg
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
    call getvtx(' ', 'SYME', 1, iarg, 1,&
                syme, iret)
!
!     INDICE DE LA LEVRE A CONSIDERER
    ifl = 0
!
!     DIRECTION TANGENTE AU POINT ORIGINE
    call jeexin(resu//'.DTAN_ORIGINE', itano)
    if (itano .ne. 0) call jeveuo(resu//'.DTAN_ORIGINE', 'L', jtano)
!
!     DIRECTION TANGENTE AU POINT EXTREMITE
    call jeexin(resu//'.DTAN_EXTREMITE', itane)
    if (itane .ne. 0) call jeveuo(resu//'.DTAN_EXTREMITE', 'L', jtane)
!
    call jeexin(resu//'.LEVRESUP.MAIL', ilev)
!
!     RECUPERATION DE L'ADRESSE DE LA BASE PAR SEGMENT DU FOND
    call jeveuo(basseg, 'E', jbasse)
!
!
!
!
!     1) VERIFICATION DE LA COHERENCE DES 2 VECTEURS DIRECTION
!     --------------------------------------------------------
!
!     ALPHA = ANGLE ENTRE LES 2 VECTEURS (EN DEGRES)
    s = vdir(1,1)*vdir(2,1) + vdir(1,2)*vdir(2,2) + vdir(1,3)*vdir(2,3)
!
!     ATTENTION, NE JAMAIS UTILISER LA FONCTION FORTRAN ACOS
    alpha = trigom('ACOS',s)*180.d0/r8pi()
!
!     CAS DE FOND "DOUBLE" : ON FEINTE EN FAISANT CROIRE QUE L'ON
!     EST DANS UNE CONFIG SYMETRIQUE
    call jeexin(resu//'.FOND_SUP.NOEU', iret)
    if (iret .ne. 0) syme='OUI'
!
!     CAS SYMETRIQUE, OU CAS DES FONDS DOUBLES
    if (syme .eq. 'OUI') then
!
!       ANGLE DOIT ETRE EGAL A 180+-2,5 DEGRES, SINON CA VEUT DIRE
!       QUE L'HYPOTHESE DE LEVRES COLLEES EST FAUSSE : ON PLANTE
        if (abs(alpha-180.d0) .gt. angmax) call u2mess('F', 'RUPTURE0_34')
!
    else if (syme.eq.'NON') then
!
!       ANGLE DOIT ETRE EGAL A 0+-5 DEGRES, SINON CA VEUT DIRE
!       QUE L'HYPOTHESE DE LEVRES COLLEES EST FAUSSE : ON PLANTE
        if (abs(alpha) .gt. 2.d0*angmax) call u2mess('F', 'RUPTURE0_34')
!
    endif
!
!
!     2) DANS LE CAS SYMETRIQUE, RECHERCHE DU BON VECTEUR DIRECTION
!     CETTE OPERATION N'EST FAITE QU'UNE SEULE FOIS POUR ISEG=1
!     -------------------------------------------------------------
!
    if (syme .eq. 'OUI' .and. iseg .eq. 1) then
!
!       CAS OU LES LEVRES SONT DONNEES
        if (ilev .ne. 0) then
!
            call jeveuo(resu//'.LEVRESUP.MAIL', 'L', jmale)
            call jelira(resu//'.LEVRESUP.MAIL', 'LONUTI', nblev)
!
            call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
!         BOUCLE SUR LES MAILLES DES LEVRES POUR TROUVER LE BON COTE
            do 200 i = 1, nblev
                call jenonu(jexnom(noma//'.NOMMAI', zk8(jmale-1 + i)), iret)
                call jeveuo(jexnum(noma//'.CONNEX', iret), 'L', iamase)
                ityp = iatyma-1+iret
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
                call dismoi('F', 'NBNO_TYPMAIL', type, 'TYPE_MAILLE', nn,&
                            k8b, iret)
                do 210 inp = 1, 2
                    compt=0
                    do 220 j = 1, nn
                        do 230 ino = 1, nbnoel
                            if (zi(iamase-1 + j) .eq. noe(indr(inp),ino)) then
                                compt = compt+1
                            endif
230                      continue
220                  continue
!             ON A TROUVE UNE FACE COINCIDENTE A UNE LEVRE, ON SORT
                    if (compt .eq. nbnoel) then
                        ifl = inp
                        goto 300
                    endif
210              continue
200          continue
!
!       SI LES LEVRES NE SONT PAS DONNEES, ON TENTE AVEC DTAN_ORIG
        else if (itano.ne.0) then
!
!         LE VECTEUR DIRECTION RETENU EST CELUI DANS LE SENS DE DTAN_ORI
            s = zr(jtano)*vdir(1,1) + zr(jtano+1)*vdir(1,2) + zr( jtano+2)*vdir(1,3)
!
            if (s .ge. 0d0) then
                ifl=1
            else
!           ON EST SUR QUE CELUI CI EST BON CAR L'ANGLE EST ENVIRON 180
                ifl=2
            endif
!
!       SINON, ON PLANTE CAR ON NE SAIT PAS QUELLE DIRECTION CHOISIR
        else
            call u2mess('F', 'RUPTURE0_8')
        endif
!
    endif
!
300  continue
!
!     3) CALCUL DES VRAIS VECTEURS DIRECTION ET NORMAL
!     ------------------------------------------------
!
!     CAS OU IL FAUT PRENDRE LA MOYENNE DES 2 VECTEURS
    if (syme .eq. 'NON') then
!
        ndir = sqrt(&
               ( vdir(1,1)+vdir(2,1))**2 + (vdir(1,2)+vdir(2,2)) **2 + (vdir(1,3)+vdir(2,3) )**2)
!
        nnor = sqrt(&
               ( vnor(1,1)+vnor(2,1))**2 + (vnor(1,2)+vnor(2,2)) **2 + (vnor(1,3)+vnor(2,3) )**2)
!
        do 310 i = 1, ndim
            vecdir(i) = (vdir(1,i)+vdir(2,i))/ndir
            vecnor(i) = sens*(vnor(1,i)+vnor(2,i))/nnor
310      continue
!
!       LE VECTEUR NORMAL DOIT ALLER DE LA LEVRE INF
!       VERS LA LEVRE SUP
        if ((iseg.eq.1) .and. (ilev.ne.0)) then
            p = ddot(ndim,vecnor,1,vect,1)
            if (p .lt. 0.d0) then
                sens = -1.d0
                do 320 i = 1, ndim
                    vecnor(i) = sens*vecnor(i)
320              continue
            endif
        endif
!
!     CAS OU IL NE FAUT PRENDRE QU'UN SEUL VECTEUR
    else if (syme.eq.'OUI') then
!
!       POUR LE 1ER VECTEUR, ON CONNAIT DEJA IFL
        if (iseg .eq. 1) then
!
            ifl = ifl
!
!       POUR LES AUTRES SEGMENTS, ON PROCEDE PAR CONTINUITE
!       DES VDIR AVEC LE VECTEUR PRECEDENT
        else if (iseg.gt.1) then
!
            prvd1 = vdir(1,1)*zr(jbasse-1+2*ndim*(iseg-1-1)+1+ndim) + vdir(1,2)*zr(jbasse-1+2*ndi&
                    &m*(iseg-1-1)+2+ndim) + vdir(1, 3)*zr(jbasse-1+2*ndim*(iseg-1-1)+3+ndim)
!
            prvd2 = vdir(2,1)*zr(jbasse-1+2*ndim*(iseg-1-1)+1+ndim) + vdir(2,2)*zr(jbasse-1+2*ndi&
                    &m*(iseg-1-1)+2+ndim) + vdir(2, 3)*zr(jbasse-1+2*ndim*(iseg-1-1)+3+ndim)
!
            if (prvd1 .gt. 0) then
!           LE VECTEUR VDIR PRECEDENT EST EN CONFORMITE AVEC IFL=1
                ifl=1
            else if (prvd2.gt.0) then
!           LE VECTEUR VDIR PRECEDENT EST EN CONFORMITE AVEC IFL=2
                ifl=2
            else
                ASSERT(.false.)
            endif
!
        endif
!
        ASSERT(ifl.ne.0)
!
        ndir = sqrt( vdir(ifl,1)**2 + vdir(ifl,2)**2 + vdir(ifl,3)**2 )
!
        nnor = sqrt( vnor(ifl,1)**2 + vnor(ifl,2)**2 + vnor(ifl,3)**2 )
!
        do 330 i = 1, ndim
            vecdir(i) = vdir(ifl,i)/ndir
            vecnor(i) = sens*vnor(ifl,i)/nnor
330      continue
!
!       LE VECTEUR NORMAL DOIT ALLER DE LA LEVRE INF
!       VERS LA LEVRE SUP
        if ((iseg.eq.1) .and. (ilev.ne.0)) then
            p = ddot(ndim,vecnor,1,vect,1)
            if (p .lt. 0.d0) then
                sens = -1.d0
                do 340 i = 1, ndim
                    vecnor(i) = sens*vecnor(i)
340              continue
            endif
        endif
!
    endif
!
!
!     4) VERIFICATION DE LA COHERENCE DE DTAN_ORIG/EXTR ET AFFECTATION
!     ----------------------------------------------------------------
!
!     SI DTAN_ORIG EST DONNE, ON VERIFIE QU'IL EST DANS LE BON SENS
    if (itano .ne. 0 .and. iseg .eq. 1) then
        s = ddot(ndim,zr(jtano),1,vecdir,1)
        if (s .le. 0.d0) call u2mesr('A', 'RUPTURE0_35', 3, vecdir)
        do 410 i = 1, ndim
            vecdir(i) = zr(jtano-1+i)
410      continue
    endif
!
!     SI DTAN_EXTR EST DONNE, ON VERIFIE QU'IL EST DANS LE BON SENS
    if (itane .ne. 0 .and. iseg .eq. nseg) then
        s = ddot(ndim,zr(jtane),1,vecdir,1)
        if (s .le. 0.d0) call u2mesr('A', 'RUPTURE0_36', 3, vecdir)
        do 420 i = 1, ndim
            vecdir(i) = zr(jtane-1+i)
420      continue
    endif
!
!
!     5) ECRITURE DE LA BASE PAR SEGMENT DU FOND
!     -------------------------------------------
!
    do 510 i = 1, ndim
        zr(jbasse-1+2*ndim*(iseg-1)+i) = vecnor(i)
        zr(jbasse-1+2*ndim*(iseg-1)+i+ndim) = vecdir(i)
510  continue
!
!
!     6) VERIF QUE LE VECTEUR NORMAL N'EST PAS TROP DIFFERENT DU
!     VECTEUR NORMAL DU SEGMENT PRECEDENT (ON TOLERE 10 DEGRES)
!     ---------------------------------------------------------
!
    if (iseg .gt. 1) then
!       RECUP DU VECTEUR NORMAL PRECEDENT
        do 610 i = 1, ndim
            vnprec(i) = zr(jbasse-1+2*ndim*(iseg-2)+i)
610      continue
        s = ddot(ndim,vecnor,1,vnprec,1)
        beta = trigom('ACOS',s)*180.d0/r8pi()
        if (abs(beta) .gt. 10.d0) call u2mess('A', 'RUPTURE0_61')
    endif
!
    call jedema()
end subroutine
