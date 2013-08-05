subroutine tldlg3(metrez, renum, istop, lmat, ildeb,&
                  ilfin, ndigit, ndeci, isingu, npvneg,&
                  iret, solvop)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
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
!    BUT  FACTORISER UNE MATRICE ASSEMBLEE
!         DIAGNOSTIQUER LES SINGULARITES OU LE NBRE DE PIVOTS NEGATIFS
!         POUR LES SOLVEURS LINEAIRES: LDLT, MULT_FRONT, MUMPS
!
!     IN  METRES :  /'LDLT' /'MULT_FRONT'/'MUMPS'
!     IN  RENUM :  /'MD' /'MDA' /'METIS' /' ' (POUR MULT_FRONT)
!     IN  ISTOP :  /0 -> SI IRET>0 : ERREUR <F>
!                  /1 -> SI IRET=1 : ALARME <A>
!                        SI IRET=2 : ERREUR <F>
!                  /2 -> LE PROGRAMME NE S'ARRETE PAS
!                        ET N'IMPRIME AUCUN MESSAGE.
!     IN  LMAT  : DESCRIPTEUR DE LA MATRICE A FACTORISER
!     IN  ILDEB : NUMERO DE LA LIGNE DE DEPART DE FACTORISATION
!     IN  ILFIN : NUMERO DE LA LIGNE DE FIN    DE FACTORISITION
!    OUT  IRET : CODE RETOUR :
!                  /0 -> OK
!                  /1 -> LE NOMBRE DE DECIMALES PERDUES SUR LE
!                        TERME DIAGONAL DE L'EQUATION ISINGU
!                        EST SUPERIEUR A NDIGIT
!                  /2 -> LA FACTORISATION N'A PAS PU SE FAIRE
!                        JUSQU'AU BOUT.(ARRET A LA LIGNE ISINGU)
!                        SI UN PIVOT DEVIENT (EN MODULE) INFERIEUR
!                        A EPS=/1./R8GAEM()
!    OUT  NPVNEG : NOMBRE DE PIVOTS NEGATIFS SUR LA MATRICE
!                  FACTORISEE.
!                  CE NOMBRE N'A DE SENS QUE SI LA MATRICE EST
!                  DE TYPE REEL ET QUE IRET<2
!     IN  NDIGIT: NOMBRE MAX DE DECIMALES A PERDRE SUR LES TERMES
!                 DIAGONAUX DE LA MATRICE
!              SI NDIGIT <0 ON NE TESTE PAS LA SINGULARITE AVEC MUMPS
!                 SI NDIGIT=0 ON PREND LA VALEUR PAR DEFAUT :8
!                 SI NDIGIT EST GRAND (99 PAR EX.) ON N'AURA
!                    JAMAIS D'ALARME.
!    OUT  NDECI : NOMBRE MAX DE DECIMALES PERDUES SUR LES TERMES
!                 DIAGONAUX DE LA MATRICE (OUTPUT ACTIVE SI
!                 NDIGIT >=0 ET SI METRES NON MUMPS)
!    OUT  ISINGU: NUMERO DE L'EQUATION OU LA PERTE DE DECIMALES
!                 EST MAXIMUM OU BIEN NUMERO DE L'EQUATION POUR
!                 LA QUELLE LA FACTORISATION S'EST ARRETEE
!    IN SOLVOP: SD_SOLVEUR DE L'OPERATEUR (PARFOIS DIFFERENT DE CELUI
!               ASSOCIE AU MATRICE). CELA SERT UNIQUEMENT A
!               MUMPS POUR LEQUEL SEUL CE JEU DE PARAMETRES FAIT FOI SI
!               IL EST DIFFERENT DE CELUI DES MATRICES.
!     ------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/amumph.h"
#include "asterfort/assert.h"
#include "asterfort/diagav.h"
#include "asterfort/dismoi.h"
#include "asterfort/imppiv.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mlfc16.h"
#include "asterfort/mtmchc.h"
#include "asterfort/mulfr8.h"
#include "asterfort/rgndas.h"
#include "asterfort/tldlc8.h"
#include "asterfort/tldlr8.h"
#include "asterfort/tlduc8.h"
#include "asterfort/tldur8.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/ualfcr.h"
    character(len=1) :: codmes, kbid
    character(len=19) :: noma19, ligrel, stolci, solvop
    character(len=14) :: nu
    character(len=*) :: metrez, renum
    character(len=8) :: nomno, nomcmp, tyddl
    character(len=16) :: metres
    character(len=24) :: kpiv
    character(len=40) :: infobl, valk(7)
    integer :: istop, lmat, ildeb, ilfin, ndigit, ndigi2, iret, npvneg, iretz
    integer :: ifm, niv, jrefa, nom, neq, iscbl, iscdi, lliai, iretp, npvnez
    integer :: typvar, typsym, nbbloc, ilfin1, ibid
    integer :: ieq3, isingu, ischc, ieq, ndeci, jdigs, npivot
    integer :: ndeci1, ndeci2, ieq4, nzero, vali(2), ipiv
    real(kind=8) :: eps, dmax, dmin, d1, rbid
    complex(kind=8) :: cbid
!     ------------------------------------------------------------------
    call jemarq()
    nom=zi(lmat+1)
    neq=zi(lmat+2)
    typvar=zi(lmat+3)
    typsym=zi(lmat+4)
    noma19=zk24(nom)(1:19)
    metres=metrez
    ieq4=0
    iretz=0
    npivot=0
!
!
!
    if (metres .ne. 'LDLT' .and. metres .ne. 'MULT_FRONT' .and. metres .ne. 'MUMPS') then
        call u2mess('F', 'ALGELINE4_1')
    endif
!
!     -- DDLS ELIMINES :
    call jeveuo(noma19//'.REFA', 'L', jrefa)
    ASSERT(zk24(jrefa-1+3).ne.'ELIMF')
    if (zk24(jrefa-1+3) .eq. 'ELIML') call mtmchc(noma19, 'ELIMF')
    ASSERT(zk24(jrefa-1+3).ne.'ELIML')
!
    call dismoi('F', 'NOM_NUME_DDL', noma19, 'MATR_ASSE', ibid,&
                nu, ibid)
    ASSERT(nu.ne.' ')
    ASSERT(zk24(jrefa-1+2)(1:14).eq.nu)
!
    call infdbg('FACTOR', ifm, niv)
    if (niv .eq. 2) then
        write (ifm,*)'<FACTOR> FACTORISATION DE LA MATRICE :',noma19
        if (typsym .eq. 1) write (ifm,*)'<FACTOR> MATRICE SYMETRIQUE'
        if (typsym .eq. 0) write (ifm,*)'<FACTOR> MATRICE NON-SYMETRIQUE'
        if (typvar .eq. 1) write (ifm,*)'<FACTOR> MATRICE REELLE'
        if (typvar .eq. 2) write (ifm,*)'<FACTOR> MATRICE COMPLEXE'
        write (ifm,*)'<FACTOR> METHODE '//metres
    endif
!
!
!     -- VALEUR DE NDIGIT PAR DEFAUT : 8
    if (ndigit .eq. 0) then
        ndigi2=8
    else
        ndigi2=ndigit
    endif
!     -- ON NE PERMET PAS LE DEBRANCHEMENT DE LA RECHERCHE DE SINGU
!        LARITE AVEC LDLT ET MULT_FRONT (POUR L'INSTANT)
    if (metres .ne. 'MUMPS') ndigi2=abs(ndigi2)
!
    if (ilfin .lt. ildeb .or. ilfin .gt. neq) then
        ilfin1=neq
    else
        ilfin1=ilfin
    endif
!
!     ON ALLOUE (SI NECESSAIRE) UN VECTEUR QUI CONTIENDRA
!     LA DIAGONALE "AVANT" ET LA DIAGONALE "APRES" :
    if (metres .ne. 'MUMPS') call diagav(noma19, neq, ilfin1, typvar, eps)
!
!
    if (metres .eq. 'LDLT') then
!     ---------------------------------------
!       -- ALLOCATION DE LA MATRICE FACTORISEE (.UALF)  ET RECOPIE
!          DE .VALM DANS .UALF
        if ((noma19.ne.'&&OP0070.RESOC.MATC') .and. (noma19.ne.'&&OP0070.RESUC.MATC')) then
            call ualfcr(noma19, ' ')
        endif
        call jelira(noma19//'.UALF', 'NMAXOC', nbbloc, kbid)
!
        stolci=nu//'.SLCS'
        call jeveuo(stolci//'.SCDI', 'L', iscdi)
        call jeveuo(stolci//'.SCBL', 'L', iscbl)
        call jeveuo(stolci//'.SCHC', 'L', ischc)
        if (typvar .eq. 1) then
            if (typsym .eq. 1) then
                call tldlr8(noma19, zi(ischc), zi(iscdi), zi(iscbl), npivot,&
                            neq, nbbloc, ildeb, ilfin1, eps)
            else if (typsym.eq.0) then
                call tldur8(noma19, zi(ischc), zi(iscdi), zi(iscbl), npivot,&
                            neq, nbbloc/2, ildeb, ilfin1, eps)
            endif
!
        else if (typvar.eq.2) then
            if (typsym .eq. 1) then
                call tldlc8(noma19, zi(ischc), zi(iscdi), zi(iscbl), npivot,&
                            neq, nbbloc, ildeb, ilfin1, eps)
            else if (typsym.eq.0) then
                call tlduc8(noma19, zi(ischc), zi(iscdi), zi(iscbl), npivot,&
                            neq, nbbloc/2, ildeb, ilfin1, eps)
            endif
        endif
!
!
    else if (metres.eq.'MULT_FRONT') then
!     ---------------------------------------
        if (typvar .eq. 1) then
            call mulfr8(noma19, npivot, neq, typsym, eps,&
                        renum)
        else if (typvar.eq.2) then
            call mlfc16(noma19, npivot, neq, typsym, eps,&
                        renum)
        endif
!
!
    else if (metres.eq.'MUMPS') then
!     ---------------------------------------
        call amumph('DETR_OCC', solvop, noma19, rbid, cbid,&
                    ' ', 0, iretz, .true.)
        call amumph('PRERES', solvop, noma19, rbid, cbid,&
                    ' ', 0, iretz, .true.)
        nzero=-999
        iretp=0
        kpiv='&&AMUMP.PIVNUL'
        if (iretz .eq. 2) then
!     -- LA FACTORISATION S'EST PAS BIEN PASSEE. PEUT IMPORTE LA VALEUR
!        NPREC ET L'ACTIVATION OU NON DE LA RECHERCHE DE SINGULARITE.
!        MATRICE SINGULIERE NUMERIQUEMENT OU EN STRUCTURE (DETECTE EN
!        AMONT DS AMUMPH. ON NE SAIT PAS PRECISER ISINGUCONTRAIREMENT A
!        MF/LDLT. ON MET ISINGU=-999 ARBITRAIREMENT)
            ndeci=-999
            isingu=-999
            npivot=-999
        else
            if (ndigi2 .gt. 0) then
!     -- ON RECUPERE LE TABLEAU DONNANT SUR LA LISTE DES PIVOTS NON-NULS
!        IL N'EXISTE QUE SI NPREC>=0 ET SI IRETZ=0
                call jeexin(kpiv, iretp)
                if (iretp .ne. 0) then
                    call jeveuo(kpiv, 'L', ipiv)
                else
                    ASSERT(.false.)
                endif
!    -- LE PREMIER ELEMENT DU TABLEAU CORRESPOND A INFOG(28)
!    -- IL INDIQUE LE NOMBRE DE PIVOTS INFERIEUR A UN CERTAIN SEUIL
!       DEFINI DANS AMUMPR
!    -- SI INFOG(28) > 0 ALORS IRETZ=1 ( LE NOMBRE DE DECIMALES PERDUES
!         SUR LE TERME DIAGONAL DE L'EQUATION ISINGU> A NDIGIT)
!    -- ATTENTION ON N'EST PAS RIGOUREUSEMENT IDENTIQUE AU CRITERE
!       HABITUEL EMPLOYE AVEC MF ET LDLT. AVEC MUMPS, LE CRITERE
!          - UTILISE LA NORME INFINIE DE LA LIGNE OU DE LA COLONNE
!            DU PIVOT ET NON PAS EXPLICITEMENT LE RAPPORT DE TERMES
!            DIAGONAUX
!          - EST GLOBAL A TOUTE LA MATRICE ET NON LOCAL PAR LIGNE
!          - ON NE DETECTE PAS LE NUMERO DE LIGNE DE PIVOT VRAIMENT NUL
!            (CAS IRETZ=2 PAS EXPLOIE ICI MAIS DIRECTEMENT DS AMUMPR/C
!             AVEC LES ERREURS MUMPS INFO(1)=-10)
!
!     -- LA FACTORISATION S'EST BIEN PASSEE. ON CHERCHE LES SINGULARITES
                if (zi(ipiv) .eq. 0) then
!             -- PAS DE SINGULARITE
                    iretz=0
                    ndeci=-999
                    isingu=-999
                    npivot=-zi(ipiv+1)
                else if (zi(ipiv).gt.0) then
!             -- AU MOINS UNE SINGULARITE
                    iretz=1
                    ndeci=ndigi2
                    isingu=zi(ipiv+2)
                    ASSERT(isingu.gt.0 .and. isingu.le.neq)
                    npivot=-zi(ipiv+1)
                else
                    ASSERT(.false.)
                endif
            else
!     -- LA FACTO S'EST BIEN PASSEE ET ON NE CHERCHE PAS A TESTER LES
!        EVENTUELLES SINGULARITES
                ndeci=-999
                isingu=-999
                npivot=-999
            endif
        endif
        if (iretp .ne. 0) call jedetr(kpiv)
    endif
!
!
!     -- CALCUL DE NPVNEG :
!     ---------------------
    if (npivot .lt. 0) then
        npvnez=npivot
    else
        npvnez=0
    endif
!
!
!     -- CALCUL DU CODE RETOUR: IRETZ,NDECIET ISINGU:
!     ------------------------------------------------
    if (metres(1:5) .ne. 'MUMPS') then
        if (npivot .gt. 0) then
            iretz=2
            ndeci=-999
            isingu=npivot
            ASSERT(isingu.gt.0 .and. isingu.le.neq)
        else
!
!     -- ON REGARDE CE QUE SONT DEVENUS LES TERMES DIAGONAUX :
!     -------------------------------------------------------
            call jeveuo(noma19//'.DIGS', 'L', jdigs)
            dmax=0.d0
            dmin=r8maem()
            nzero=0
            do 10 ieq = ildeb, ilfin1
                if (typvar .eq. 1) then
                    d1=abs(zr(jdigs-1+ieq)/zr(jdigs+neq-1+ieq))
                else
                    d1=abs(zc(jdigs-1+ieq)/zc(jdigs+neq-1+ieq))
                endif
                if (d1 .gt. dmax) then
                    dmax=d1
                    ieq3=ieq
                endif
                if (d1 .eq. 0.d0) then
                    nzero=nzero+1
                else
                    if (d1 .lt. dmin) then
                        dmin=d1
                        ieq4=ieq
                    endif
                endif
10          continue
            ASSERT(dmax.gt.0)
            ndeci1=int(log10(dmax))
            ndeci2=int(log10(1.d0/dmin))
            ndeci=ndeci1
            isingu=ieq3
            ASSERT(isingu.gt.0 .and. isingu.le.neq)
            if (ndeci .ge. ndigi2) then
                iretz=1
            else
                iretz=0
            endif
        endif
    endif
!
!
!
!     -- EMISSION EVENTUELLE D'UN MESSAGE D'ERREUR :
!     ----------------------------------------------
    if ((ndigi2.lt.0) .and. (metres.eq.'MUMPS')) goto 30
    if (istop .eq. 2) then
        goto 20
!
    else if (iretz.eq.0) then
        goto 20
!
    else if (istop.eq.1) then
        if (iretz .eq. 1) then
            codmes='A'
        else if (iretz.eq.2) then
            codmes='F'
        else
            ASSERT(.false.)
        endif
    else if (istop.eq.0) then
        codmes='F'
    endif
!
!
    ASSERT(isingu.eq.-999 .or. isingu.gt.0)
    vali(1)=isingu
!
    ASSERT(ndeci.eq.-999 .or. ndeci.ge.0)
    if (isingu .eq. -999) ASSERT(ndeci.eq.-999)
    vali(2)=ndeci
!
    valk(1)='XXXX'
    valk(2)='XXXX'
    valk(3)='XXXX'
    valk(4)='XXXX'
    valk(5)='XXXX'
    valk(6)='XXXX'
    valk(7)='XXXX'
!
!
    if (isingu .gt. 0) then
        call rgndas(nu, isingu, nomno, nomcmp, tyddl,&
                    ligrel, infobl)
        valk(4)=tyddl
        if (tyddl .eq. 'A') then
            ASSERT(nomcmp.ne.'LAGR')
            valk(1)=nomno
            valk(2)=nomcmp
        else if (tyddl.eq.'B') then
!         -- SI C'EST UN SIMPLE BLOCAGE DE DDL PHYSIQUE :
            valk(3)=infobl
!
        else if (tyddl.eq.'C') then
!         -- SI C'EST UN NOEUD DE LAGRANGE D'UNE LIAISON_DDL
!            ON IMPRIME LES NOEUDS CONCERNES PAR LA LIAISON :
            lliai=index(infobl,'LIAISON_DDL')
            ASSERT(lliai.gt.0)
            call imppiv(nu, isingu)
!
        else if (tyddl.eq.'D') then
!           -- CAS NUME_DDL_GENE :
            valk(5)=nomno
            valk(6)=nomcmp
            valk(7)=infobl
        else
            ASSERT(.false.)
        endif
    endif
    call u2mesg(codmes, 'FACTOR_10', 7, valk, 2,&
                vali, 0, 0.d0)
!
!
20  continue
!     -- IMPRESSIONS INFO=2 :
!     ------------------------
    if (niv .eq. 2) then
        write (ifm,*)'<FACTOR> APRES FACTORISATION :'
        if (nzero .gt. 0) then
            write (6,*)'<FACTOR> MATRICE NON DEFINIE POSITIVE.'
            write (6,*)'<FACTOR> IL EXISTE ',nzero,&
     &      ' ZEROS SUR LA DIAGONALE.'
        endif
        write (ifm,*)'<FACTOR>  NB MAX. DECIMALES A PERDRE :',ndigi2
        write (ifm,*)'<FACTOR>  NB DECIMALES PERDUES       :',ndeci
        write (ifm,*)'<FACTOR>  NUM. EQUATION LA PIRE      :',isingu
        write (ifm,*)'<FACTOR>  NOMBRE PIVOTS NEGATIFS     :',-npvnez
        write (ifm,*)'<FACTOR>  CODE ARRET (ISTOP)         :',istop
        write (ifm,*)'<FACTOR>  CODE RETOUR (IRET)         :',iretz
!
!       -- ALARME EVENTUELLE SI LE PIVOT DEVIENT TROP GRAND :
        if ((metres.ne.'MUMPS') .and. (ndeci2.ge.ndigi2)) then
            ASSERT(ieq4.gt.0 .and. ieq4.le.neq)
            write (ifm,*)'<FACTOR> PROBLEME FACTORISATION.'
            write (ifm,*)'<FACTOR> LE PIVOT DEVIENT TRES GRAND',&
            ' A LA LIGNE:',ieq4
            write (ifm,*)'<FACTOR> NOMBRE DE DECIMALES PERDUES:',&
            ndeci2
            call rgndas(nu, ieq4, nomno, nomcmp, tyddl,&
                        ligrel, infobl)
            write (ifm,*)'<FACTOR> NOEUD:',nomno,' CMP:',nomcmp
        endif
    endif
!
!
30  continue
!
!
    iret=iretz
    npvneg=npvnez
    call jedema()
!
end subroutine
