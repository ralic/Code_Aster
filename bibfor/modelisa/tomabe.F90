subroutine tomabe(chmat, nmabet, nbmabe, mailla, nbnoma,&
                  mail2d, nbnobe, nunobe, xflu, xret,&
                  regl)
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!  DESCRIPTION : CARACTERISATION DE LA TOPOLOGIE DE LA STRUCTURE BETON
!  -----------   ET RECUPERATION DES CARACTERISTIQUES DU MATERIAU
!                CONSTITUTIF
!                APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
!
!  IN     : CHMAT  : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT CHAM_MATER ASSOCIE A L'ETUDE
!  IN     : NMABET : CHARACTER*24 ,
!                    OBJET CONTENANT LES MAILLES BETON
!  IN     : NBMABE : INTEGER , SCALAIRE
!                    NOMBRE DE MAILLE BETON
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : NBNOMA : INTEGER , SCALAIRE
!                    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
!  OUT    : MAIL2D : LOGICAL , SCALAIRE
!                    INDICATEUR BOOLEEN CARACTERISANT LA TOPOLOGIE DE LA
!                    STRUCTURE BETON
!                    SI ( MAIL2D ) : REPRESENTATION PAR DES MAILLES 2D
!                    SINON         : REPRESENTATION PAR DES MAILLES 3D
!  OUT    : NBNOBE : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS APPARTENANT A LA STRUCTURE BETON
!  IN     : NUNOBE : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
!                    NUMEROS DES NOEUDS APPARTENANT A LA STRUCTURE BETON
!  OUT    : XFLU   : REAL*8 , SCALAIRE
!                    VALEUR DU TAUX DE PERTE DE TENSION PAR FLUAGE DU
!                    BETON, EN % DE LA TENSION INITIALE
!  OUT    : XRET   : REAL*8 , SCALAIRE
!                    VALEUR DU TAUX DE PERTE DE TENSION PAR RETRAIT DU
!                    BETON, EN % DE LA TENSION INITIALE
!  OUT    : REGL   : CHARACTER*4 TYPE DE REGLEMENT UTILISE :
!                    BPEL OU ETCC
!
!  N.B. LE VECTEUR NUNOBE EST REMPLI LORS DU PASSAGE DANS LA ROUTINE
!       TOMABE, APRES AJUSTEMENT DE SA DIMENSION A NBNOBE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
#include "jeveux.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
! ARGUMENTS
! ---------
    character(len=8) :: chmat, mailla
    integer :: nbnoma, nbnobe, nbmabe
    logical(kind=1) :: mail2d
    character(len=19) :: nunobe
    real(kind=8) :: xflu, xret
    character(len=24) :: nmabet
!
! VARIABLES LOCALES
! -----------------
    integer :: ias, icste, idecal, imail, ino, iret, jconx, jncoch, jnumab
    integer :: jnunob, jptma, jtyma, jvalk, jvalr, nbconx, nbcste, ntyma, numail
    integer :: numnoe, jcesd, jcesl,  iad
!
    integer :: ntri3, ntri6, nqua4, nqua8, nqua9, ntet4, ntet10, npyr5, npyr13
    integer :: npen6, npen15, nhex8, nhex20, nhex27
!
    logical(kind=1) :: mail3d, trouv1, trouv2
    character(len=3) :: k3mai
    character(len=8) :: beton
    character(len=19) :: carte, nomrc, chsmat, cartez, chtmp
    character(len=24) :: captma, cavalk, conxma, rcvalk, rcvalr, tymama
!
    character(len=8) :: bpelb(2)
    character(len=4) :: regl
    real(kind=8) :: crite
    character(len=8), pointer :: cesv(:) => null()
    data          bpelb  /'PERT_FLU','PERT_RET'/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   CARACTERISATION DE LA TOPOLOGIE DE LA STRUCTURE BETON :
!     REPRESENTATION PAR DES MAILLES 2D OU 3D
!     VERIFICATION DU TYPE DE CHAQUE MAILLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 1.1 ACCES AUX OBJETS DU CONCEPT MAILLAGE
! ---
    conxma = mailla//'.CONNEX'
    call jeveuo(nmabet, 'L', jnumab)
    tymama = mailla//'.TYPMAIL'
    call jeveuo(tymama, 'L', jtyma)
!
! 1.2 SAISIE DES TYPES DE MAILLES ACCEPTABLES POUR UNE REPRESENTATION 2D
! --- ET VERIFICATION
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3'), ntri3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA6'), ntri6)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4'), nqua4)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD8'), nqua8)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD9'), nqua9)
    mail2d = .true.
    do 10 imail = 1, nbmabe
        numail = zi(jnumab+imail-1)
        ntyma = zi(jtyma+numail-1)
        if ((ntyma.ne.ntri3) .and. (ntyma.ne.ntri6) .and. ( ntyma.ne.nqua4) .and.&
            (ntyma.ne.nqua8) .and. (ntyma.ne.nqua9)) then
            mail2d = .false.
            goto 11
        endif
10  end do
11  continue
!
! 1.3 SAISIE DES TYPES DE MAILLES ACCEPTABLES POUR UNE REPRESENTATION 3D
! --- ET VERIFICATION
!
    if (mail2d) then
        mail3d = .false.
    else
        call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA4' ), ntet4)
        call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA10'), ntet10)
        call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM5' ), npyr5)
        call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM13'), npyr13)
        call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA6' ), npen6)
        call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA15'), npen15)
        call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA8' ), nhex8)
        call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA20' ), nhex20)
        call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA27' ), nhex27)
        mail3d = .true.
        do 20 imail = 1, nbmabe
            numail = zi(jnumab+imail-1)
            ntyma = zi(jtyma+numail-1)
            if ((ntyma.ne.ntet4) .and. (ntyma.ne.ntet10) .and. (ntyma.ne.npyr5) .and.&
                (ntyma.ne.npyr13) .and. ( ntyma.ne.npen6) .and. (ntyma.ne.npen15) .and.&
                ( ntyma.ne.nhex8) .and. (ntyma.ne.nhex20) .and. ( ntyma.ne.nhex27)) then
                mail3d = .false.
                goto 21
            endif
20      continue
21      continue
    endif
!
! 1.4 SORTIE EN ERREUR FATALE SI REPRESENTATION NON ACCEPTABLE
! ---
    if ((.not.mail2d) .and. (.not.mail3d)) then
        call utmess('F', 'MODELISA7_46')
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   DETERMINATION DES NOEUDS APPARTENANT A LA STRUCTURE BETON
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 2.1 CREATION D'UN OBJET DE TRAVAIL
! ---
    call wkvect('&&TOMABE.NOE_COCHES', 'V V I', nbnoma, jncoch)
    do 30 ino = 1, nbnoma
        zi(jncoch+ino-1) = 0
30  end do
!
! 2.2 LECTURE DES CONNECTIVITES DES MAILLES
! ---
    do 40 imail = 1, nbmabe
        numail = zi(jnumab+imail-1)
        call jelira(jexnum(conxma, numail), 'LONMAX', nbconx)
        call jeveuo(jexnum(conxma, numail), 'L', jconx)
        do 41 ino = 1, nbconx
            numnoe = zi(jconx+ino-1)
            zi(jncoch+numnoe-1) = zi(jncoch+numnoe-1) + 1
41      continue
40  end do
!
! 2.3 DECOMPTE DES NOEUDS ET RELEVE DE LEUR NUMERO
! ---
    nbnobe = 0
    do 50 ino = 1, nbnoma
        if (zi(jncoch+ino-1) .gt. 0) nbnobe = nbnobe + 1
50  end do
!
    call jeecra(nunobe, 'LONUTI', nbnobe)
    call jeveuo(nunobe, 'E', jnunob)
    idecal = 0
    do 60 ino = 1, nbnoma
        if (zi(jncoch+ino-1) .gt. 0) then
            idecal = idecal + 1
            zi(jnunob+idecal-1) = ino
        endif
60  end do
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4   RECUPERATION DU MATERIAU CONSTITUTIF DE LA STRUCTURE BETON
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    carte = chmat//'.CHAMP_MAT '
    cavalk = carte//'.VALE'
    captma = carte//'.PTMA'
    call jeveuo(cavalk, 'L', jvalk)
    call jeveuo(captma, 'L', jptma)
!
    numail = zi(jnumab)
    ias = zi(jptma+numail-1)
    if (ias .eq. 0) then
        write(k3mai,'(I3)') numail
        call utmess('F', 'MODELISA7_47', sk=k3mai)
    endif
!
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 5.  RECUPERATION DES CARACTERISTIQUES DU MATERIAU CONSTITUTIF
!     DE LA STRUCTURE BETON ET VERIFICATION DE LA COMPATIBILITE DES
!     MATERIAUX BETON.
!     ( LA LOI BPEL_BETON OU ETCC_BETON DOIT ETRE LA MEME
!      POUR TOUTES LES MAILLES )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!
!     TRANSFORMATION DU CHAM_MATER EN CHAM_ELEM_S POUR TRAITEMENT
!
    chsmat='&&TOMABE.NOM_MATER'
    cartez='&&TOMABE.CARTE'
    chtmp='&&TOMABE.BID'
!
    call copisd(' ', 'V', carte, cartez)
    call carces(cartez, 'ELEM', chtmp, 'V', chsmat,&
                'A', iret)
    call jeveuo(chsmat//'.CESD', 'L', jcesd)
    call jeveuo(chsmat//'.CESL', 'L', jcesl)
    call jeveuo(chsmat//'.CESV', 'L', vk8=cesv)
!
    numail = zi(jnumab+1-1)
    call cesexi('C', jcesd, jcesl, numail, 1,&
                1, 1, iad)
!
!.... RELATION DE COMPORTEMENT <BPEL_BETON> OU  <ETCC_BETON>
!      ON TESTE D'ABORD SI BPEL_BETON EST DONNE (ON A IMPOSE DANS
!     LE CATALOGUE QU'UN SEUL COMPORTEMENT ETAIT POSSIBLE)
!
    regl='BPEL'
    beton=cesv(iad)
    nomrc = beton//'.BPEL_BETON'
    rcvalk = nomrc//'.VALK'
    call jeexin(rcvalk, iret)
    if (iret .eq. 0) then
!       ON TESTE SI ETCC_BETON EST RENSEIGNE
        regl='ETCC'
        nomrc = beton//'.ETCC_BETON'
        rcvalk = nomrc//'.VALK'
        call jeexin(rcvalk, iret)
        if (iret .eq. 0) then
            call utmess('F', 'MODELISA7_48')
        endif
    endif
!
!     RECUPERATION DES PERTES PAR FLUAGE OU RETRAIT POUR BPEL_BETON
    if (regl .eq. 'BPEL') then
        rcvalr = nomrc//'.VALR'
        call jeveuo(rcvalk, 'L', jvalk)
        call jeveuo(rcvalr, 'L', jvalr)
        call jelira(rcvalr, 'LONMAX', nbcste)
        trouv1 = .false.
        trouv2 = .false.
        do 150 icste = 1, nbcste
            if (zk8(jvalk+icste-1) .eq. bpelb(1)) then
                trouv1 = .true.
                xflu = zr(jvalr+icste-1)
            endif
            if (zk8(jvalk+icste-1) .eq. bpelb(2)) then
                trouv2 = .true.
                xret = zr(jvalr+icste-1)
            endif
            if (trouv1 .and. trouv2) goto 151
150      continue
!
!
!
151      continue
!
! CRITERE DE COMPARAISON DES VALEURS MATERIAUX INTRODUITES
! PAR L UTILISATEUR DANS LA RELATION BPEL_BETON
! QUI DOIT ETRE UNIQUE A TOUT LE BETON
!
        crite=1.d-07
        if (nbmabe .gt. 1) then
            do 200 imail = 2, nbmabe
                numail = zi(jnumab+imail-1)
                call cesexi('C', jcesd, jcesl, numail, 1,&
                            1, 1, iad)
                beton=cesv(iad)
                nomrc = beton//'.BPEL_BETON'
                rcvalk = nomrc//'.VALK'
                rcvalr = nomrc//'.VALR'
                call jeveuo(rcvalk, 'L', jvalk)
                call jeveuo(rcvalr, 'L', jvalr)
                call jelira(rcvalr, 'LONMAX', nbcste)
!
                ias = zi(jptma+numail-1)
                if (ias .eq. 0) then
                    write(k3mai,'(I3)') numail
                    call utmess('F', 'MODELISA7_47', sk=k3mai)
                endif
!
                do 250 icste = 1, nbcste
!
                    if (zk8(jvalk+icste-1) .eq. bpelb(1)) then
                        if (abs(xflu) .lt. crite) then
                            if (abs(xflu-zr(jvalr+icste-1)) .gt. crite) then
                                call utmess('F', 'MODELISA7_49')
                            endif
                        else
                            if (abs((xflu-zr(jvalr+icste-1))/xflu) .gt. crite) then
                                call utmess('F', 'MODELISA7_49')
                            endif
                        endif
                    else if (zk8(jvalk+icste-1).eq.bpelb(2)) then
                        if (abs(xret) .lt. crite) then
                            if (abs(xret-zr(jvalr+icste-1)) .gt. crite) then
                                call utmess('F', 'MODELISA7_51')
                            endif
                        else
                            if (abs((xret-zr(jvalr+icste-1))/xret) .gt. crite) then
                                call utmess('F', 'MODELISA7_51')
                            endif
                        endif
                    endif
250              continue
200          continue
        endif
!
        if (.not. ( trouv1 .and. trouv2 )) then
            call utmess('F', 'MODELISA7_52')
        endif
        if (( xflu.lt.0.0d0 ) .or. ( xret.lt.0.0d0 ) .or. ( xflu+ xret.gt.1.0d0 )) then
            call utmess('F', 'MODELISA7_53')
        endif
!
!        CALL JELIRA(RCVALR,'LONMAX',NBCSTE,K1B)
!        write(6,*)'tomabe',RCVALK,RCVALR,NBCSTE
!        CALL JEVEUO(RCVALR,'L',JVALR)
    endif
!
!
!
! --- MENAGE
    call detrsd('CHAM_ELEM_S', chsmat)
    call detrsd('CARTE', '&&TOMABE.CARTE')
    call detrsd('CHAM_ELEM_S', chtmp)
    call jedema()
!
! --- FIN DE TOMABE.
!
end subroutine
