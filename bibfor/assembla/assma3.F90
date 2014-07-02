subroutine assma3(lmasym, lmesym, tt, igr, iel,&
                  c1, rang, jnueq, jnumsd, jresl,&
                  jrsvi, nbvel, nnoe, ldist, ldgrel,&
                  ilima, jadli, jadne, jprn1, jprn2,&
                  jnulo1, jnulo2, jposd1, jposd2, admodl,&
                  lcmodl, mode, nec, nmxcmp, ncmp,&
                  jsmhc, jsmdi, iconx1, iconx2, jtmp2,&
                  lgtmp2, jvalm, ilinu, ellagr, exivf,&
                  jdesc, jrepe, jptvoi, jelvoi, codvoi)
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
! person_in_charge: jacques.pellet at edf.fr
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
! BUT : ASSEMBLER UN ELEMENT FINI
!-----------------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/ascopr.h"
#include "asterfort/asret2.h"
#include "asterfort/asretm.h"
#include "asterfort/assert.h"
#include "asterfort/corddl.h"
#include "asterfort/utmess.h"
#include "asterfort/voiuti.h"
    aster_logical :: lmasym, lmesym
    character(len=*) :: exivf
    character(len=2) :: tt
    real(kind=8) :: c1
    integer :: iel, admodl, rang, iconx1, iconx2, jadli, jadne
    integer :: i1, i2, iad1, iad11, iad2, iad21
    integer :: igr, ilima, ilinu, nbterm
    integer :: jnueq, jnulo1, jnumsd, jposd1, jprn1, jprn2
    integer :: jresl, jrsvi, jsmdi, jsmhc, jtmp2, jvalm(2), lgtmp2
    integer :: lcmodl, k1, k2, n2, n3, jnulo2, jposd2
    integer :: mode, n1, nbvel, ncmp, nddl1, nddl2
    integer :: nec, nmxcmp, nnoe, numa, nk2, decael, jdesc
    aster_logical :: ldist, ldgrel
!
    character(len=16) :: codvoi
    integer :: nvoima, nscoma, jrepe, jptvoi, jelvoi, nbvois
    parameter(nvoima=100,nscoma=4)
    integer :: livois(1:nvoima), tyvois(1:nvoima), nbnovo(1:nvoima)
    integer :: nbsoco(1:nvoima), lisoco(1:nvoima, 1:nscoma, 1:2)
    integer :: nbi1mx, nbi1
    parameter (nbi1mx=27*27*10*10)
    integer :: ti1(nbi1mx)
    integer :: ti2(nbi1mx)
!-----------------------------------------------------------------------
!     FONCTIONS FORMULES :
!-----------------------------------------------------------------------
    integer :: ellagr
    integer :: nnov, igr2, mode2, numav, kvois
!
#define zzconx(imail,j) zi(iconx1-1+zi(iconx2+imail-1)+j-1)
#define zzliel(ili,igrel,j) zi(zi(jadli+3*(ili-1)+1)-1+ \
    zi(zi(jadli+3*(ili-1)+2)+igrel-1)+j-1)
#define zznema(ili,iel,j) zi(zi(jadne+3*(ili-1)+1)-1+ \
    zi(zi(jadne+3*(ili-1)+2)+iel-1)+j-1)
#define zzprno(ili,nunoel,l) zi(jprn1-1+zi(jprn2+ili-1)+ \
    (nunoel-1)*(nec+2)+l-1)
!
#define numlo1(kno,k) zi(jnulo1-1+2*(kno-1)+k)
#define numlo2(kno,k) zi(jnulo2-1+2*(kno-1)+k)
#define posdd1(kno,kddl) zi(jposd1-1+nmxcmp*(kno-1)+kddl)
#define posdd2(kno,kddl) zi(jposd2-1+nmxcmp*(kno-1)+kddl)
!----------------------------------------------------------------------
!
    nbterm=0
    nbi1=0
!     NUMA : NUMERO DE LA MAILLE
    numa=zzliel(ilima,igr,iel)
!
!
!
!     -- SI LES CALCULS ONT ETE DISTRIBUES :
!     --------------------------------------
    if (ldist .and. .not.ldgrel) then
!       SI ON EST DANS UN CALCUL DISTRIBUE, ON SE POSE
!       LA QUESTION DE L'APPARTENANCE DE LA MAILLE NUMA AUX
!       DONNEES ATTRIBUEES AU PROC SI MAILLE PHYSIQUE: CHAQUE PROC
!       NE TRAITE QUE CELLES ASSOCIEES AUX SD QUI LUI SONT ATTRIBUES
!       SI MAILLE TARDIVE: ELLES SONT TRAITEES PAR LE PROC 0
        if (numa .gt. 0) then
            if (zi(jnumsd-1+numa) .ne. rang) goto 110
        else
            if (rang .ne. 0) goto 110
        endif
    endif
!
!
!     ---------------------------------------------------------------
!     OBJET TEMPORAIRE .TMP2 :
!     .TMP2 : (1:2*DIM(MATR_ELEM)) POSITION RELATIVE DANS LES BLOCS
!     POUR LE I-EME REEL DE LA MATRICE ELEM :
!     TMP2(2*(I-1)+1) --> NUMERO DU BLOC OU S'INJECTE I.
!     TMP2(2*(I-1)+2) --> POSITION DANS LE BLOC DU REEL I.
!     ---------------------------------------------------------------
!
!
!
!
!     ----------------------------
!     1. CALCUL DE NDDL1 ET IAD1 :
!        + MISE A JOUR DE ELLAGR
!     ----------------------------
!
!
!     1.1. MAILLE DU MAILLAGE :
!     ------------------------
    if (numa .gt. 0) then
!
        do k1 = 1, nnoe
            n1=zzconx(numa,k1)
            iad1=zzprno(1,n1,1)
            call corddl(admodl, lcmodl, jprn1, jprn2, 1,&
                        mode, nec, ncmp, n1, k1,&
                        nddl1, zi(jposd1-1+nmxcmp*(k1-1)+1))
            ASSERT(nddl1.le.nmxcmp)
            zi(jnulo1-1+2*(k1-1)+1)=iad1
            zi(jnulo1-1+2*(k1-1)+2)=nddl1
        end do
!
!
!
!     1.2. MAILLE TARDIVE :
!     ------------------------
    else
        numa=-numa
!
!
!       MISE A JOUR DE ELLAGR :
!       LA MAILLE EST UN ELEMENT DE DUALISATION DE CL (LAGRANGE) SI:
!         - TRIA3 TARDIF (NNOE=3, NUMA <0)
!         - N1 EST UN NOEUD PHYSIQUE (>0)
!         - N2 ET N3 SONT DES NOEUDS TARDIFS PORTANT 1 CMP: 'LAGR'
!       --------------------------------------------------------------
        if ((ellagr.eq.0) .and. (nnoe.eq.3)) then
            n1=zznema(ilima,numa,1)
            n2=zznema(ilima,numa,2)
            n3=zznema(ilima,numa,3)
            if ((n1.gt.0) .and. (n2.lt.0) .and. (n3.lt.0)) then
!           -- POUR L'INSTANT ON NE VERIFIE PAS QUE N2 ET N3 NE
!              PORTENT QUE LA CMP 'LAGR'
                ellagr=1
            endif
        endif
!
!
        do k1 = 1, nnoe
!         N1 : INDICE DU NOEUDS DS LE .NEMA DU LIGREL
!              DE CHARGE GLOBAL OU LOCAL
            n1=zznema(ilima,numa,k1)
            if (n1 .lt. 0) then
!           NOEUD TARDIF
                n1=-n1
!
!
!
!           -- NUMERO D'EQUATION DU PREMIER DDL DE N1
                iad1=zzprno(ilinu,n1,1)
                call corddl(admodl, lcmodl, jprn1, jprn2, ilinu,&
                            mode, nec, ncmp, n1, k1,&
                            nddl1, zi(jposd1-1+nmxcmp*(k1-1)+1))
!
            else
!           -- NOEUD PHYSIQUE
                iad1=zzprno(1,n1,1)
                call corddl(admodl, lcmodl, jprn1, jprn2, 1,&
                            mode, nec, ncmp, n1, k1,&
                            nddl1, zi(jposd1-1+nmxcmp*(k1-1)+1))
            endif
!
            zi(jnulo1-1+2*(k1-1)+1)=iad1
            zi(jnulo1-1+2*(k1-1)+2)=nddl1
        end do
    endif
!
!
!
!     -----------------------------------------------------------
!     2. ON BOUCLE SUR LES TERMES DE LA MATRICE ELEMENTAIRE
!        POUR NOTER OU ILS DOIVENT ETRE RECOPIES
!     -----------------------------------------------------------
!
    do k1 = 1, nnoe
        iad1=numlo1(k1,1)
        nddl1=numlo1(k1,2)
        if (lmesym) then
            nk2=k1
        else
            nk2=nnoe
        endif
        do i1 = 1, nddl1
            do k2 = 1, nk2
                iad2=numlo1(k2,1)
                nddl2=numlo1(k2,2)
                if (lmesym .and. (k2.eq.k1)) nddl2=i1
                do i2 = 1, nddl2
                    iad11=zi(jnueq-1+iad1+posdd1(k1,i1)-1)
                    iad21=zi(jnueq-1+iad2+posdd1(k2,i2)-1)
                    nbi1=nbi1+1
                    ti1(nbi1)=iad11
                    ti2(nbi1)=iad21
                end do
            end do
        end do
    end do
    ASSERT(nbi1.le.nbi1mx)
    call asret2(lmasym, jtmp2, lgtmp2, nbterm, jsmhc,&
                jsmdi, nbi1, ti1, ti2)
!
!
!     -- SI LE RESUELEM EST 'VOISIN_VF', IL FAUT ENCORE ASSEMBLER LES
!        CONTRIBUTIONS DES ELEMENTS VOISINS :
!     -----------------------------------------------------------------
    if (exivf .eq. 'OUI') then
        ASSERT(.not.lmesym )
        call voiuti(numa, codvoi, nvoima, nscoma, jrepe,&
                    jptvoi, jelvoi, nbvois, livois, tyvois,&
                    nbnovo, nbsoco, lisoco)
        ASSERT(nbvois.le.30)
        do kvois = 1, nbvois
            numav=livois(kvois)
            nnov =nbnovo(kvois)
            igr2=zi(jrepe-1+2*(numav-1)+1)
            mode2=zi(jdesc+igr2+1)
!
            do k2 = 1, nnov
                n2=zzconx(numav,k2)
                iad2=zzprno(1,n2,1)
                call corddl(admodl, lcmodl, jprn1, jprn2, 1,&
                            mode2, nec, ncmp, n2, k2,&
                            nddl2, zi(jposd2-1+nmxcmp*(k2-1)+1))
                ASSERT(nddl2.le.nmxcmp)
                zi(jnulo2-1+2*(k2-1)+1)=iad2
                zi(jnulo2-1+2*(k2-1)+2)=nddl2
            end do
!
!
            do k1 = 1, nnoe
                iad1=numlo1(k1,1)
                nddl1=numlo1(k1,2)
                do i1 = 1, nddl1
                    do k2 = 1, nnov
                        iad2=numlo2(k2,1)
                        nddl2=numlo2(k2,2)
                        do i2 = 1, nddl2
                            iad11=zi(jnueq-1+iad1+posdd1(k1,i1)-1)
                            iad21=zi(jnueq-1+iad2+posdd2(k2,i2)-1)
                            call asretm(lmasym, jtmp2, lgtmp2, nbterm, jsmhc,&
                                        jsmdi, iad11, iad21)
                        end do
                    end do
                end do
            end do
!
        end do
    endif
!
!
!
!     -----------------------------------------------------------
!     3. ON RECOPIE EFFECTIVEMENT LES TERMES:
!        (NBTERM CONTIENT LE NOMBRE DE TERMES (R/C) A TRAITER)
!     -----------------------------------------------------------
    if (exivf .eq. 'OUI') then
        decael=zi(jrsvi-1+iel)-1
    else
        decael=nbvel*(iel-1)
    endif
    call ascopr(lmasym, lmesym, tt, jtmp2, nbterm,&
                jresl+decael, c1, jvalm)
!
110 continue
end subroutine
