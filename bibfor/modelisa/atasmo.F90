subroutine atasmo(neq, az, apddl, apptr, numedz,&
                  ataz, basez, nblia, nmul, numatz)
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
    implicit none
!
!     ATASMO  --  LE BUT DE CETTE ROUTINE EST DE CREER LA MATR_ASSE
!                 DE NOM ATA.
!                 LE .VALM DE CETTE MATR_ASSE VA CONTENIR LES TERMES
!                 DU PRODUIT DE MATRICES AT*A.
!                 A EST 'CONCEPTUELLEMENT' UNE MATRICE RECTANGLE
!                 DE 'HAUTEUR' NBLIG ET DE LARGEUR NEQ.
!                 A EST 'INFORMATIQUEMENT' UNE COLLECTION NUMEROTEE
!                 COMPORTANT NBLIG OBJETS QUI SONT DES VECTEURS DE REELS
!                 DE LONGUEUR NEQ.
!                 CHACUN DE CES VECTEURS EST UNE LIGNE DE LA MATRICE A.
!                 LA MATR_ASSE ATA VA DONC ETRE SYMETRIQUE ET A
!                 VALMURS REELLES. D'AUTRE PART ON VA LUI AFFECTER
!                 UN PROFIL MORSE.
!
!   ARGUMENT        E/S  TYPE         ROLE
!    AZ              IN    K*     NOM DE LA COLLECTION DES VECTEURS
!                                 LIGNES (I.E. AZ EST LA MATRICE
!                                 RECTANGULAIRE POUR LAQUELLE ON VA
!                                 CALCULER LE PRODUIT AZ_T*AZ).
!    NUMEDZ         IN    K*      NOM DU NUME_DDL DECRIVANT LES
!                                 LIGNES DE LA MATRICE AZ
!    BASEZ           IN    K*     NOM DE LA BASE SUR LAQUELLE ON
!                                 CREE LA MATR_ASSE.
!    ATAZ           OUT    K*     NOM DE LA MATR_ASSE SYMETRIQUE
!                                 A VALMURS REELLES DONT LE .VALM
!                                 CONTIENT LE PRODUIT AT*A.
!                                 LE PROFIL DE CETTE MATRICE EST
!                                 EN LIGNE DE CIEL.
!                                 CE PROFIL EST DETERMINE DANS LA
!                                 ROUTINE.
!    NUMATZ         OUT    K*     NOM DU NUME_DDL A CREER POUR ATAZ
!                                 ON LE DETRUIT S'Il EXISTE DEJA
!.========================= DEBUT DES DECLARATIONS ====================
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/moinip.h"
#include "asterfort/moinsr.h"
#include "asterfort/trir.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: ma
! -----  ARGUMENTS
    character(len=*) :: az, numedz, ataz, basez, numatz
    integer :: neq, nblia, nmul
    integer :: apddl(*), apptr(*)
! -----  VARIABLES LOCALES
    integer :: j, k, iimax, jhbid, idsuiv, dimacv,  jconl, nblig, nblig2
    integer ::  ilig, idligm, nddlt, jacv, jaci, iilib, idlm, iddl
    integer :: ieq, jsmhc, jsmdi, ncoef, jvalm, decal, jrefa
    integer :: i, jsmde, ii1, ii2, iii, ii, jj, jdecal, nddltm, kdeb
    character(len=1) :: base
    character(len=14) :: numddl, numedd
    character(len=19) :: ata
    character(len=24) :: ksmhc, ksmdi, krefa, kconl, kvalm
    character(len=24) :: a, ksuiv, khbid
    real(kind=8) :: un, vi, vj, vij
    integer, pointer :: acompac_1er(:) => null()
    integer, pointer :: acompac_nbt(:) => null()
!
! ========================= DEBUT DU CODE EXECUTABLE ==================
    call jemarq()
!
!
! --- 1. INITIALISATIONS :
!     ---------------------
    base = basez
    a = az
    ata = ataz
    numedd = numedz
    nblig = nblia*nmul
    numddl = numatz
    call detrsd('MATR_ASSE', ata)
    call detrsd('NUME_DDL', numddl)
    call copisd('NUME_EQUA', base, numedd//'.NUME', numddl//'.NUME')
!     UNE DROLE DE GLUTE A RESORBER :
    call jedup1(numedd//'.MLTF.RENU', base, numddl//'.MLTF.RENU')
    call jedup1(numedd//'.NSLV', base, numddl//'.NSLV')
!
    un = 1.0d0
!
!
    call jelira(a, 'NMAXOC', nblig2)
    ASSERT(nblig.gt.0)
    ASSERT(nblig.le.nblig2)
!
    ksmdi = numddl//'.SMOS.SMDI'
    call wkvect(ksmdi, base//' V I', neq, jsmdi)
!
!
!     IIMAX   LONGUEUR MAXIMUM ADMISSIBLE DE KHBID ET ISUIV
!             DANS LA ROUTINE MOINSR IIMAX EST AUGMENTE SI NECESS.
!     KHBID   TABLE DES NUMEROS DE LIGNE
!     ISUIV   TABLE DES CHAINAGES DE LA STRUCTURE CHAINEE
!            (SMDI,SMHC,ISUIV) QUI EST CONTRUITE AVANT D'OBTENIR LA
!               STRUCTURE COMPACTE (SMDI,SMHC) DE LA MATRICE .
    khbid = '&&ATASMO.SMOS.SMHC'
    ksuiv = '&&ATASMO.ANCIEN.ISUIV'
!     ON COMMENCE AVEC IIMAX=100
    iimax = 100
    call wkvect(khbid, 'V V I', iimax, jhbid)
    call wkvect(ksuiv, 'V V I', iimax, idsuiv)
!
!
!     2. CALCUL DE DIMACV
!        DIMACV= NOMBRE TOTAL DE TERMES NON NULS DANS A
!        -- ALLOCATION DE &&ATASMO.ACOMPAC_NBT
!        -- ALLOCATION DE &&ATASMO.ACOMPAC_1ER
!     ----------------------------------------------------------------
    dimacv = 0
    AS_ALLOCATE(vi=acompac_nbt, size=nblig)
    AS_ALLOCATE(vi=acompac_1er, size=nblig)
    do j = 1, nmul
        do ilig = 1, nblia
            call jeveuo(jexnum(a, ilig+(j-1)*nblia), 'L', idligm)
            nddltm = apptr(ilig+1) - apptr(ilig)
            nddlt = 0
            do i = 1, nddltm
                if (zr(idligm-1+i) .ne. 0.d0) nddlt = nddlt + 1
            end do
!           ASSERT(NDDLT.GT.0)
            acompac_nbt(ilig+(j-1)*nblia) = nddlt
            acompac_1er(ilig+(j-1)*nblia) = dimacv + 1
            dimacv = dimacv + nddlt
            call jelibe(jexnum(a, ilig+(j-1)*nblia))
        end do
    end do
!       ASSERT(DIMACV.GT.0)
    dimacv = max(dimacv,1)
!
!
!     3. COMPACTAGE DE A : ON NE CONSERVE QUE LES TERMES /= 0 AINSI
!        QUE LES INDICES CORRESPONDANT :
!     ----------------------------------------------------------------
    call wkvect('&&ATASMO.ACOMPAC_I', 'V V I', dimacv, jaci)
    call wkvect('&&ATASMO.ACOMPAC_V', 'V V R', dimacv, jacv)
    k = 0
    do j = 1, nmul
        do ilig = 1, nblia
            call jeveuo(jexnum(a, ilig+(j-1)*nblia), 'L', idligm)
            nddlt = apptr(ilig+1) - apptr(ilig)
            jdecal = apptr(ilig)
            kdeb = k
            do i = 1, nddlt
                if (zr(idligm-1+i) .ne. 0.d0) then
                    k = k + 1
                    zi(jaci-1+k) = apddl(jdecal+i)
                    zr(jacv-1+k) = zr(idligm-1+i)
                endif
            end do
!         ON DOIT TRIER LE TABLEAU DES NUMEROS D'EQUATIONS
!         CAR MOINSR S'ATTEND A UN TABLEAU ORDONNE
!         LE TABLEAU DES VALEURS EST AUSSI PERMUTE POUR
!         RESPECTER CE TRI
            call trir(zi(jaci+kdeb), zr(jacv+kdeb), 1, k-kdeb)
            call jelibe(jexnum(a, ilig+(j-1)*nblia))
        end do
    end do
!
!
!     4. OBJETS DU NUME_DDL : .SMHC ET .SMDI :
!     ----------------------------------------
!     IILIB  : 1-ERE PLACE LIBRE
    iilib = 1
    call wkvect('&&ATASMO.LMBID', 'V V I', 1, idlm)
!
!     4.1 : ON FORCE LA PRESENCE DES TERMES DIAGONAUX:
    do ieq = 1, neq
        zi(idlm) = ieq
        call moinsr(zi(idlm), 1, idlm, jsmdi, idsuiv,&
                    ksuiv, jhbid, khbid, iilib, iimax)
    end do
!
!
!     4.2 : ON INSERE LES VRAIS TERMES :
    do ilig = 1, nblig
!       NDDLT : NOMBRE DE TERMES NON NULS POUR ILIG
        nddlt = acompac_nbt(ilig)
        idlm = jaci - 1 + acompac_1er(ilig)
!
!       -- INSERTION DES COLONNES DE L'ELEMENT DANS
!           LA STRUCTURE CHAINEE
        do iddl = 0, nddlt - 1
            call moinsr(zi(idlm+iddl), iddl+1, idlm, jsmdi, idsuiv,&
                        ksuiv, jhbid, khbid, iilib, iimax)
        end do
    end do
!
!
!     DESIMBRIQUATION DE CHAINES POUR OBTENIR LA STRUCTURE COMPACTE
!     (ZI(JSMDI),SMHC) DE LA MATRICE
    ksmhc = numddl//'.SMOS.SMHC'
    call wkvect(ksmhc, base//' V S', iimax, jsmhc)
    call moinip(neq, ncoef, zi(jsmdi), zi(idsuiv), zi(jhbid),&
                zi4(jsmhc))
!
!
!     5. OBJET DU NUME_DDL :  .SMDE :
!     ----------------------------------------------------
    call wkvect(numddl//'.SMOS.SMDE', base//' V I', 6, jsmde)
    zi(jsmde+1-1) = neq
    zi(jsmde+2-1) = ncoef
    zi(jsmde+3-1) = 1
!
!
!
!     6. OBJETS: MATR_ASSE.REFA ET MATR_ASSE.CONL:
!     --------------------------------------------------
    krefa = ata//'.REFA'
    call wkvect(krefa, base//' V K24', 20, jrefa)
    call dismoi('NOM_MAILLA', numedd, 'NUME_DDL', repk=ma)
    zk24(jrefa-1+1) = ma
    zk24(jrefa-1+2) = numddl
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'NOEU'
    zk24(jrefa-1+11) = 'MPI_COMPLET'
    kconl = ata//'.CONL'
    call wkvect(kconl, base//' V R', neq, jconl)
    do i = 1, neq
        zr(jconl+i-1) = un
    end do
!
!
!     7. OBJET: MATR_ASSE.VALM :
!     --------------------------------------------------
    kvalm = ata//'.VALM'
    call jecrec(kvalm, base//' V R', 'NU', 'DISPERSE', 'CONSTANT',&
                1)
    call jeecra(kvalm, 'LONMAX', ncoef)
    call jecroc(jexnum(kvalm, 1))
    call jeveuo(jexnum(kvalm, 1), 'E', jvalm)
    do ilig = 1, nblig
!       NDDLT : NOMBRE DE TERMES NON NULS POUR ILIG
        nddlt = acompac_nbt(ilig)
        idlm = jaci - 1 + acompac_1er(ilig)
        decal = acompac_1er(ilig)
!
!       -- CALCUL DE .VALM(II,JJ) :
        do j = 1, nddlt
            vj = zr(jacv-1+decal-1+j)
            jj = zi(jaci-1+decal-1+j)
            ASSERT(jj.le.neq)
            ii2 = zi(jsmdi-1+jj)
            if (jj .eq. 1) then
                ii1 = 1
            else
                ii1 = zi(jsmdi-1+jj-1) + 1
            endif
            ASSERT(ii2.ge.ii1)
            do i = 1, j
                vi = zr(jacv-1+decal-1+i)
                ii = zi(jaci-1+decal-1+i)
                vij = vi*vj
!           -- CUMUL DE VIJ DANS .VALM :
                do iii = ii1, ii2
                    if (zi4(jsmhc-1+iii) .eq. ii) goto 110
                end do
                ASSERT(.false.)
110             continue
                zr(jvalm-1+iii) = zr(jvalm-1+iii) + vij
            end do
        end do
    end do
!
!
!     9. MENAGE :
!     ------------
    call jedetr('&&ATASMO.SMOS.SMHC')
    call jedetr('&&ATASMO.ANCIEN.ISUIV')
    AS_DEALLOCATE(vi=acompac_nbt)
    AS_DEALLOCATE(vi=acompac_1er)
    call jedetr('&&ATASMO.ACOMPAC_V')
    call jedetr('&&ATASMO.ACOMPAC_I')
    call jedetr('&&ATASMO.LMBID')
!
    call jedema()
end subroutine
