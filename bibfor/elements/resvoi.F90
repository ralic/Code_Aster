subroutine resvoi(moz, maz, chvoiz)
    implicit none
!
! DECLARATION PARAMETRES D'APPEL
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/celver.h"
#include "asterfort/cncinv.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
    character(len=*) :: moz, maz, chvoiz
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! ......................................................................
!    - FONCTION REALISEE:  RECHERCHE DES VOISINS DES ELEMENTS D'UN
!                          MAILLAGE 2D OU 3D (MAILLES DE BORD COMPRISES)
!                          ON REMPLIT CHVOIS
!
!    - ARGUMENTS:
!       IN (JXIN)           MOZ            -->  MODELE
!       IN                  MAZ            -->  NOM DU MAILLAGE
!       IN (JXVAR)          CHVOIZ         -->  CHAM_ELEM VOISIN
!
! ......................................................................
!
! DECLARATION VARIABLES LOCALES
!
    character(len=32) :: noe1, noe2, noe3, noe4
    character(len=24) :: typmai, connex, coninv, valk(1), nomma
    character(len=8) :: ma, kbid, typema, mo
    character(len=19) :: ligrmo, chvois
    integer :: ibid, nbvois
    integer :: ibidt(1), vali(2)
    integer :: nbno, nbma, nbs, nbf, tymvol
    integer :: ima, ino, ino1, ino2, ino3, ino4, kma, jma
    integer :: iamav1, iamav2, iamav3, iamav4, iarepe, iavale
    integer :: ifa, ima1, ima2, ima3, ima4, ier
    integer :: igrel, iel, igrelv, ielv
    integer :: iaval1, iaval2, jad, iad, iadv
    integer :: jceld, nbmav1, nbmav2, nbmav3, nbmav4
    integer :: numav1, numav2, numav3, numav4, typ, som(4, 6, 4), iatyma
!
    real(kind=8) :: r8bidt(1)
!
    logical :: troisd
!
    integer :: debugr
    character(len=1) :: k1bid
!
!   INITIALISATION DES NUMEROS DE SOMMETS DES FACES D'ELEMENTS 3D
!     SOM (IN,IFA,TYMVOL) : IN     : NUMERO DU SOMMET DANS LA FACE
!                           IFA    : NUMERO DE LA FACE
!                           TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
!                                    1 : HEXAEDRES
!                                    2 : PENTAEDRES
!                                    3 : TETRAEDRES
!                                    4 : PYRAMIDES
!  ==> POUR LE IN-EME SOMMET DE LA IFA-EME FACE D'UNE MAILLE DE TYPE
!      TYMVOL, SOMMET (IN,IFA,TYMVOL) EST SON NUMERO LOCAL DANS LA
!      DESCRIPTION DE LA MAILLE VOLUMIQUE.
!      ON RAPPELLE QU'EN FORTRAN L'ORDRE DE RANGEMENT EST LE SUIVANT :
!   (1,1,1) (2,1,1) (3,1,1) (4,1,1) (1,2,1) (2,2,1) ... (4,2,1)
!   (1,3,1)  ...    (3,6,4) (4,6,4)
!    ON COMMENCE AINSI PAR LES 4 SOMMETS DE LA 1ERE FACE DE L'HEXAEDRE,
!    PUIS LES 4 SOMMETS DE LA 2EME FACE DE L'HEXAEDRE,
!    ETC JUSQU'AUX 4 SOMMETS DE LA 6EME FACE DE L'HEXAEDRE.
!    ENSUITE ON A LES 3 SOMMETS DE LA 1ERE FACE DU PENTAEDRE, ETC
!    ON CHOISIT UNE ORIENTATION ENTRANTE POUR DECRIRE UNE FACE
!     VOIR TE003 POUR LES EXPLICATIONS DETAILLEES
!
    data som/1,2,3,4, 1,5,6,2, 2,6,7,3, 3,7,8,4, 4,8,5,1, 5,8,7,6,&
     &         1,2,3,0, 4,6,5,0, 1,4,5,2, 2,5,6,3, 1,3,6,4, 0,0,0,0,&
     &         1,2,3,0, 2,4,3,0, 3,4,1,0, 1,4,2,0, 0,0,0,0, 0,0,0,0,&
     &         1,5,2,0, 2,5,3,0, 3,5,4,0, 4,5,1,0, 1,2,3,4, 0,0,0,0/
!
! --------- CONSTRUCTION DE LA CONNECTIVITE INVERSE --------------------
!
    call jemarq()
    chvois = chvoiz
    mo = moz
    ma = maz
    call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbno,&
                kbid, ier)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ier)
!
    typmai = ma//'.TYPMAIL'
    connex = ma//'.CONNEX'
!
! --------- RECHERCHE DES EVENTUELLES MAILLES 3D DANS LE MODELE --------
!
    troisd = .false.
    call jeveuo(typmai, 'L', iatyma)
    do 500 ima = 1, nbma
        iad=iatyma-1+ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
        if (typema(1:2) .eq. 'TE' .or. typema(1:2) .eq. 'PE' .or. typema(1:2) .eq. 'HE') then
            troisd = .true.
            goto 600
        endif
500  end do
    if (nbma .lt. 1) goto 700
!
!
! --------- CREATION DU POINTEUR DE LONGUEUR DE CONINV ----------------
!
600  continue
    coninv = '&&RESVOI.CONINV'
    call cncinv(ma, ibidt, 0, 'G', coninv)
!
! ----------- RECHERCHE DES ADRESSES DE STOCKAGE POUR CHVOIS -------
!
    ligrmo = mo//'.MODELE'
    call jeveuo(ligrmo//'.REPE', 'L', iarepe)
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(chvois, 'NBVARI_CST', 'STOP', ibid)
    call celver(chvois, 'NBSPT_1', 'STOP', ibid)
!
    call jeveuo(chvois//'.CELD', 'L', jceld)
    call jeveuo(chvois//'.CELV', 'E', iavale)
!
!   RECHERCHE DES VOISINS DE CHAQUE MAILLE
!
    if (troisd) then
!
!    CAS 3D
!
! ----------- BOUCLE SUR LES MAILLES -------------------------------
!
        do 801 , ima = 1 , nbma
!
        igrel = zi(iarepe-1+2*(ima-1)+1)
        iel = zi(iarepe-1+2*(ima-1)+2)
        if ((igrel.eq.0) .and. (iel.eq.0)) goto 801
!
        call jeveuo(jexnum(connex, ima), 'L', jad)
        iad=iatyma-1+ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
!
        if (typema(1:4) .eq. 'HEXA') then
            nbf = 6
            tymvol = 1
        else if (typema(1:4) .eq. 'PENT') then
            nbf = 5
            tymvol = 2
        else if (typema(1:4) .eq. 'TETR') then
            nbf = 4
            tymvol = 3
        else if (typema(1:4) .eq. 'PYRA') then
            nbf = 5
            tymvol = 4
!GN        WRITE(6,*) '. MAILLE ',IMA
        else
            goto 801
        endif
!
        igrel = zi(iarepe-1+2*(ima-1)+1)
        iel = zi(iarepe-1+2*(ima-1)+2)
        if (iel .eq. 0) goto 801
        debugr=zi(jceld-1+zi(jceld-1+4+igrel)+8)
        iaval1 = iavale - 1 + debugr
        iaval2 = iaval1 + 14*(iel-1)
!
! ---------- BOUCLE SUR LES FACES DE LA MAILLE -------------------
!      NBMAVI = NOMBRE DE MAILLES POSSEDANT LE I-EME SOMMET
!               S'IL N'Y EN A QU'UNE, C'EST LA MAILLE COURANTE DONC
!               IL N'Y A PAS DE VOISINS PAR CETTE FACE : ON PASSE
!               A LA FACE SUIVANTE.
!
        do 802 , ifa = 1 , nbf
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '.. FACE NUMERO ',IFA
        ino1 = som(1,ifa,tymvol)
        noe1 = jexnum(coninv,zi(jad-1+ino1))
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... INO1 ',INO1
        call jelira(noe1, 'LONMAX', nbmav1, k1bid)
        if (nbmav1 .eq. 1) goto 802
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... NBMAV1 ',NBMAV1
        call jeveuo(noe1, 'L', iamav1)
!
        ino2 = som(2,ifa,tymvol)
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... INO2 ',INO2
        noe2 = jexnum(coninv,zi(jad-1+ino2))
        call jelira(noe2, 'LONMAX', nbmav2, k1bid)
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... NBMAV2 ',NBMAV2
        if (nbmav2 .eq. 1) goto 802
        call jeveuo(noe2, 'L', iamav2)
!
        ino3 = som(3,ifa,tymvol)
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... INO3 ',INO3
        noe3 = jexnum(coninv,zi(jad-1+ino3))
        call jelira(noe3, 'LONMAX', nbmav3, k1bid)
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... NBMAV3 ',NBMAV3
        if (nbmav3 .eq. 1) goto 802
        call jeveuo(noe3, 'L', iamav3)
!
!AS DES FACES QUADRANGULAIRES
        ino4 = som(4,ifa,tymvol)
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... INO4 ',INO4
        if (ino4 .ne. 0) then
            noe4 = jexnum(coninv,zi(jad-1+ino4))
            call jelira(noe4, 'LONMAX', nbmav4, k1bid)
!GN        IF (TYPEMA(1:4).EQ.'PYRA' ) WRITE(6,*) '... NBMAV4 ',NBMAV4
            if (nbmav4 .eq. 1) goto 802
            call jeveuo(noe4, 'L', iamav4)
        endif
!
! REPERAGE ET STOCKAGE DES VOISINS
!  BOUCLE 803 : ON REGARDE TOUTES LES MAILLES POSSEDANT LE 1ER SOMMET
!               DE LA MAILLE COURANTE, IMA, ET QUI NE SONT PAS IMA
!
        nbvois = 0
        do 803 , ima1 = 1,nbmav1
        numav1 = zi(iamav1-1+ima1)
        if (numav1 .ne. ima) then
!
!  BOUCLE 804 : ON REGARDE TOUTES LES MAILLES POSSEDANT LE 2EME SOMMET
!               DE LA MAILLE COURANTE
!               ON RETIENT CELLES QUI POSSEDENT AUSSI LE PREMIER
!
            do 804 , ima2 = 1,nbmav2
            numav2 = zi(iamav2-1+ima2)
            if (numav2 .eq. numav1) then
!
!  BOUCLE 805 : ON REGARDE TOUTES LES MAILLES POSSEDANT LE 3EME SOMMET
!               DE LA MAILLE COURANTE
!               ON RETIENT CELLES QUI POSSEDENT AUSSI LE PREMIER
!               ELLES POSSEDENT AUSSI LE DEUXIEME.
!
                do 805 , ima3 = 1,nbmav3
                numav3 = zi(iamav3-1+ima3)
                if (numav3 .eq. numav1) then
!
!       CAS DES FACES QUADRANGULAIRES
!  BOUCLE 806 : ON REGARDE TOUTES LES MAILLES POSSEDANT LE 4EME SOMMET
!               DE LA MAILLE COURANTE
!               ON RETIENT CELLES QUI POSSEDENT AUSSI LE PREMIER
!               ELLES POSSEDENT AUSSI LE DEUXIEME ET LE TROISIEME.
                    if (ino4 .ne. 0) then
!
                        do 806 ima4 = 1, nbmav4
                            numav4 = zi(iamav4-1+ima4)
                            if (numav4 .eq. numav1) then
!
!- ------STOCKAGE DU NUMERO DU VOISIN ET DE SON TYPE ----------------
!
!                         SI LA MAILLE N'EST PAS DANS LE MODELE, ON SORT
                                igrelv = zi(iarepe-1+2*( numav1-1)+1)
                                ielv = zi(iarepe-1+2*(numav1- 1)+2)
                                if ((igrelv.eq.0) .and. ( ielv.eq.0)) goto 803
!
                                nbvois = nbvois + 1
!
                                if (nbvois .gt. 1) goto 803
!
                                zi(iaval2+ifa) = numav1
                                iadv=iatyma-1+numav1
                                typ = zi(iadv)
                                zi(iaval2+ifa+7) = typ
                                goto 803
                            endif
806                      continue
!
                    else
!
!       CAS DES FACES TRIANGULAIRES
!
!-------STOCKAGE DU NUMERO DU VOISIN ET DE SON TYPE ----------------
!                       SI LA MAILLE N'EST PAS DANS LE MODELE, ON SORT
                        igrelv = zi(iarepe-1+2*( numav1-1)+1)
                        ielv = zi(iarepe-1+2*(numav1- 1)+2)
                        if ((igrelv.eq.0) .and. ( ielv.eq.0)) goto 803
!
                        nbvois = nbvois + 1
!
                        if (nbvois .gt. 1) goto 803
!
                        zi(iaval2+ifa) = numav1
                        iadv=iatyma-1+numav1
                        typ = zi(iadv)
                        zi(iaval2+ifa+7) = typ
                        goto 803
!
                    endif
                endif
!
805              continue
            endif
!
804          continue
!
        endif
!
803      continue
        if (nbvois .gt. 1) then
            call jenuno(jexnum(ma//'.NOMMAI', ima), nomma)
            valk(1)=nomma
            vali(1)=ifa
            vali(2)=nbvois
            call u2mesg('F', 'INDICATEUR_12', 1, valk, 2,&
                        vali, 0, r8bidt(1))
            call assert(.false.)
        endif
802      continue
!
! --------- STOCKAGE DU NUMERO DE L'ELEMENT ET DE SON TYPE -------------
!
        zi(iaval2) = ima
        typ = zi(iad)
        zi(iaval2+7) = typ
!
        801     end do
!
!
    else
!
!    CAS 2D
!
! ----------- BOUCLE SUR LES MAILLES -------------------------------
!
        do 601 ima = 1, nbma
!
!       SI LA MAILLE N'EST PAS DANS LE MODELE, ON SORT
            igrel = zi(iarepe-1+2*(ima-1)+1)
            iel = zi(iarepe-1+2*(ima-1)+2)
            if ((igrel.eq.0) .and. (iel.eq.0)) goto 601
!
            call jeveuo(jexnum(connex, ima), 'L', jad)
!
            iad=iatyma-1+ima
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
            if (typema(1:4) .eq. 'QUAD') then
                nbs = 4
            else if (typema(1:4) .eq. 'TRIA') then
                nbs = 3
            else
                goto 601
            endif
!
            igrel = zi(iarepe-1+2*(ima-1)+1)
            iel = zi(iarepe-1+2*(ima-1)+2)
            if (iel .eq. 0) goto 601
            debugr=zi(jceld-1+zi(jceld-1+4+igrel)+8)
            iaval1 = iavale - 1 + debugr
!
! ---------- BOUCLE SUR LES SOMMETS DE LA MAILLE -------------------
!
            do 602 ino = 1, nbs
                call jelira(jexnum(coninv, zi(jad-1+ino)), 'LONMAX', nbmav1, k1bid)
                call jeveuo(jexnum(coninv, zi(jad-1+ino)), 'L', iamav1)
!
                nbvois = 0
                do 603 kma = 1, nbmav1
                    numav1 = zi(iamav1-1+kma)
                    if (numav1 .ne. ima) then
!
                        if (ino .eq. nbs) then
                            ino2 = 1
                        else
                            ino2 = ino + 1
                        endif
!
                        call jelira(jexnum(coninv, zi(jad-1+ino2)), 'LONMAX', nbmav2, k1bid)
                        call jeveuo(jexnum(coninv, zi(jad-1+ino2)), 'L', iamav2)
!
                        do 604 jma = 1, nbmav2
                            numav2 = zi(iamav2-1+jma)
                            if (numav2 .eq. numav1) then
!
! --------- STOCKAGE DU NUMERO DU VOISIN ET DE SON TYPE ----------------
!
!                 SI LA MAILLE N'EST PAS DANS LE MODELE, ON SORT
                                igrelv = zi(iarepe-1+2*(numav1-1)+1)
                                ielv = zi(iarepe-1+2*(numav1-1)+2)
                                if ((igrelv.eq.0) .and. (ielv.eq.0)) goto 603
!
                                nbvois = nbvois + 1
!
                                if (nbvois .gt. 1) goto 603
!
                                zi(iaval1+14*(iel-1)+ino) = numav1
                                iadv=iatyma-1+numav1
                                typ = zi(iadv)
                                zi(iaval1+14*(iel-1)+ino+7) = typ
!
                                goto 603
                            endif
604                      continue
                    endif
603              continue
                if (nbvois .gt. 1) then
                    call jenuno(jexnum(ma//'.NOMMAI', ima), nomma)
                    valk(1)=nomma
                    vali(1)=ino
                    vali(2)=nbvois
                    call u2mesg('F', 'INDICATEUR_12', 1, valk, 2,&
                                vali, 0, r8bidt(1))
                    call assert(.false.)
                endif
602          continue
!
! --------- STOCKAGE DU NUMERO DE L'ELEMENT ET DE SON TYPE -------------
!
            zi(iaval1+14*(iel-1)) = ima
            typ = zi(iad)
            zi(iaval1+14*(iel-1)+7) = typ
601      end do
    endif
    call jedetr('&&RESVOI.LONGCONINV')
    call jedetr('&&RESVOI.CONINV')
    call jedema()
700  continue
end subroutine
