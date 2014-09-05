subroutine vechth(modelz, chargz, infocz, carelz, matez,&
                  instz, chtnz, vecelz)
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
!
! ----------------------------------------------------------------------
! CALCUL DES VECTEURS ELEMENTAIRES DES CHARGES THERMIQUES
!
! IN  MODELE : NOM DU MODELE
! IN  CHARGE : LISTE DES CHARGES
! IN  INFOCH : INFORMATIONS SUR LES CHARGEMENTS
! IN  CARELE : CHAMP DE CARA_ELEM
! IN  MATE   : CHAMP MATERIAU
! IN  INST   : CARTE CONTENANT LA VALEUR DE L'INSTANT
! . POUR LE CALCUL DE LA TEMPERATURE :
! IN  CHTN   : CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT
! OUT VECELZ : VECT_ELEM
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       MESSAGE:INFNIV.
!       JEVEUX:JEMARQ,JEDEMA,JEEXIN,JELIRA,JEVEUO,WKVECT,JEECRA,JEDETR.
!       MANIP SD: RSEXCH,DETRSD,MEGEOM,MECARA,EXISD,MECACT.
!       CALCUL: CALCUL.
!       FICH COMM: GETRES.
!       DIVERS: GCNCO2,CORICH.
!
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/exixfe.h"
#include "asterfort/gcnco2.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/wkvect.h"
#include "asterfort/xajcin.h"
    character(len=*) :: modelz, chargz, infocz, carelz, instz, chtnz, vecelz
    character(len=*) :: matez
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter (nompro='VECHTH')
    integer :: nchinx
    parameter (nchinx=16)
    integer :: iret, nchar, icha, ilve, jchar, ibid, ifm, niv, jinf, jlve, k
    integer :: lonlis, numchm, nchin, ireth, i
    character(len=8) :: nomcha, lpain(nchinx), paout, k8bid, newnom
    character(len=16) :: option, nomcmd
    character(len=24) :: ligrel(3), lchin(nchinx), resuel, chgeom, chcara(18)
    character(len=24) :: modele, charge, infoch, carele, inst, chtn, mate
    character(len=24) :: vecele, ligcal
    aster_logical :: bidon
! ----------------------------------------------------------------------
    integer :: nbchmx
    parameter (nbchmx=6)
    integer :: nbopt(nbchmx), nligr(nbchmx)
    character(len=6) :: nompar(nbchmx), nomchp(nbchmx), nomopt(nbchmx)
    character(len=19) :: chvarc
    data nomchp/'.T_EXT','.FLURE','.FLUR2','.SOURE','.HECHP','.GRAIN'/
    data nomopt/'TEXT_','FLUN_','FLUX_','SOUR_','PARO_','GRAI_'/
    data nompar/'PT_EXT','PFLUXN','PFLUXV','PSOURC','PHECHP','PGRAIN'/
    data nbopt/5,3,3,3,5,7/
    data nligr/1,1,1,1,2,1/
!
! DEB ------------------------------------------------------------------
    chvarc = '&&NXACMV.CHVARC'
!
!====
! 1.1 PREALABLES LIES AUX OPTIONS
!====
    call jemarq()
    call infniv(ifm, niv)
    newnom = '.0000000'
    modele = modelz
    charge = chargz
    infoch = infocz
    carele = carelz
    mate = matez
    inst = instz
    chtn = chtnz
    do i = 1, nchinx
        lpain(i) = '        '
        lchin(i) = '                        '
    end do
!
!====
! 1.2 PREALABLES LIES AUX CHARGES
!====
    ligrel(1) = modele(1:8)//'.MODELE'
! FLAG POUR SIGNIFIER LA PRESENCE DE CHARGES (NCHAR NON NUL)
    bidon = .true.
! TEST D'EXISTENCE DE L'OBJET JEVEUX CHARGE
    call jeexin(charge, iret)
    if (iret .ne. 0) then
! LECTURE DU NBRE DE CHARGE NCHAR DANS L'OBJET JEVEUX CHARGE
        call jelira(charge, 'LONMAX', nchar)
        if (nchar .ne. 0) then
            bidon = .false.
! LECTURE DES ADRESSES JEVEUX DES CHARGES ET DES INFOS AFFERENTES
            call jeveuo(charge, 'L', jchar)
            call jeveuo(infoch, 'L', jinf)
        endif
    endif
!
!====
! 2. INIT. DES VECTEURS SECONDS MEMBRE ELEMENTAIRES
!====
!
    vecele = '&&VETCHA'
    resuel = '&&'//nompro//'.???????'
!
! DESTRUCTION DE LA SD VECELE DE TYPE 'VECT_ELEM'
    call detrsd('VECT_ELEM', vecele)
    vecelz = vecele(1:19)//'.RELR'
! CREATION ET INITIALISATION DU VECT_ELEM ASSOCIE AU MODELE MODELE(1:8),
! AU MATERIAU MATE, AU CARA_ELEM CARELE ET A LA SUR_OPTION 'CHAR_THER'
    call memare('V', vecele(1:19), modele(1:8), mate, carele,&
                'CHAR_THER')
! CREATION DU VECTEUR VECELZ DE TYPE K24, DE LONGUEUR EFFECTIVE LONLIS
! ET DE LONGUEUR UTILE NULLE
    if (bidon) then
        call wkvect(vecelz, 'V V K24', 1, jlve)
        call jeecra(vecelz, 'LONUTI', 0)
! PAS DE CHARGE, ON S'EN VA
        goto 80
    else
! NCHAR CHARGES, ON PREPARE LES SDS DE STOCKAGE IDOINES
        lonlis = nbchmx*nchar
        call wkvect(vecelz, 'V V K24', lonlis, jlve)
        call jeecra(vecelz, 'LONUTI', 0)
    endif
!
! RECHERCHE DU CHAMP DE GEOMETRIE CHGEOM ASSOCIE AU MODELE(1:8) ET A LA
! LISTE DE CHARGES ZK24(JCHAR)
    call megeom(modele(1:8), chgeom)
! RECHERCHE DES NOMS DES CARAELEM CHCARA DANS LA CARTE CARELE(1:8)
    call mecara(carele(1:8), chcara)
!
!====
! 3. PREPARATION DES CALCULS ELEMENTAIRES
!====
!
! CHAMP LOCAL CONTENANT LA CARTE DES NOEUDS (X Y Z)
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom
! ... LA CARTE DES INSTANTS (INST DELTAT THETA KHI  R RHO)
    lpain(3) = 'PTEMPSR'
    lchin(3) = inst
! ... LE CHAM_NO DE TEMPERATURE A L'INSTANT PRECEDENT (TEMP)
    lpain(4) = 'PTEMPER'
    lchin(4) = chtn
! ... LE COEFFICIENT D'ECHANGE
    lpain(5) = 'PCOEFH'
! ... LA CARTE MATERIAU (I1)
    lpain(6) = 'PMATERC'
    lchin(6) = mate
! ... LE CHAMP DE DEPLACEMENT
    lpain(7) = 'PVARCPR'
    lchin(7) = chvarc
! ... LE CHAMP DE DEPLACEMENT
    lpain(8) = 'PDEPLAR'
    lchin(8) = '&&DEPPLU'
!
! ... LE CHAMP RESULTAT
    paout = 'PVECTTR'
!
!====
! 3.1 IMPRESSIONS NIVEAU 2 POUR DIAGNOSTICS...
!====
!
    if (niv .eq. 2) then
        write (ifm,*) '*******************************************'
        write (ifm,*) ' CALCUL DE SECOND MEMBRE THERMIQUE: ',nompro
        write (ifm,*)
        write (ifm,*) ' LIGREL/MODELE    :',ligrel(1)
        write (ifm,*) ' OBJ JEVEUX CHARGE:',charge
        write (ifm,*) '            INFOCH:',infoch
        write (ifm,*) ' NBRE DE CHARGES  :',nchar
        write (ifm,*) ' BOUCLE SUR LES CHARGES DE TYPE NEUMANN LIN'
    endif
!
!====
! 4. BOUCLE SUR LES AFFE_CHAR_THER ==================================
!====
    ilve = 0
    do icha = 1, nchar
!
!====
! 4.1 PREALABLES LIES AUX TYPES DE CL DE CET AFFE_CHAR_THER
!====
        numchm = zi(jinf+nchar+icha)
!
        if (numchm .gt. 0) then
!
! NOM DE LA CHARGE
            nomcha = zk24(jchar+icha-1) (1:8)
            ligrel(2) = nomcha//'.CHTH.LIGRE'
            if (niv .eq. 2) then
                write (ifm,*) ' '
                write (ifm,*) '   CHARGE         :',nomcha
            endif
!
!==== ASSEMBLAGE DU SECOND MEMBRE                                ====
!==== ELEMENTAIRE LIE AUX CHARGEMENTS                            ====
!
!====
! 4.2 SI ON DOIT ASSEMBLER, CONSTRUCTION AU CAS PAR CAS DE L'OPTION
!     DE CALCUL. BOUCLE SUR LES NBCHMX TYPES DE CL POSSIBLES =======
!====
!
            if (niv .eq. 2) write (ifm,* ) '     BOUCLE SUR LES TYPES DE CHARGES'
            do k = 1, nbchmx
!
! ACCES A LA DESCRIPTION '.DESC' DES CHTH.NOMCHP(K) ET CHTH.COEFH DE LA
! CHARGE NOMCHA POUR DEFINIR LES CHAMPS LOCAUX DE LA CARTE DE CHARGEMENT
                lchin(1) = nomcha(1:8)//'.CHTH'//nomchp(k)
                lchin(5) = nomcha(1:8)//'.CHTH'//'.COEFH  '
!
! TEST EXISTENCE DE L'OBJET JEVEUX POUR SAVOIR SI LE CALCUL ELEMENTAIRE
! DE TYPE TYPCAL ASSOCIE A CETTE CL NOMCHP(K) POUR CE ICHA IEME AFFE_
! CHAR_THER EST NECESSAIRE
                iret = 0
                call exisd('CHAMP_GD', lchin(1), iret)
!
! ASSEMBLAGE CAR LA CHARGE ICHA CORRESPOND A NOMCHP(K)
                if (iret .ne. 0) then
!
!====
! 4.3 TEST ET TRAITEMENT SUPPLEMENTAIRE POUR LA CONDITION ECHANGE
!====
                    if (k .eq. 1) then
                        ireth = 0
                        call exisd('CHAMP_GD', lchin(5), ireth)
                        if (((ireth.eq.0).and. (iret.ne.0)) .or.&
                            ((iret.eq.0).and. (ireth.ne.0))) then
                            ASSERT(.false.)
                        endif
                    endif
!
!====
! 4.4 PREPARATION FINALE DES OPTIONS DE CALCUL
!====
                    if (numchm .eq. 1) then
! CHAMP LOCAL CONSTANT ASSOCIE A LA CARTE NOMPAR(K)//'R'
!
                        lpain(1) = nompar(k)//'R'
                        lpain(5) = lpain(5) (1:6)//'R'
                        option = 'CHAR_THER_'//nomopt(k) (1:5)//'R'
                    else
! CHAMP LOCAL VARIABLE (EN ESPACE 2) OU (EN TEMPS ET/OU EN ESPACE 3)
!
                        lpain(1) = nompar(k)//'F'
                        lpain(5) = lpain(5) (1:6)//'F'
                        option = 'CHAR_THER_'//nomopt(k) (1:5)//'F'
                    endif
!
! ON FIXE LE NBRE DE PARAMETRES DE L'OPTION EN PRENANT LE MAX DE
! L'OPTION REELLE ET CONSTANTE
                    nchin = nbopt(k)
!
!====
! 4.5 CAS PARTICULIERS THER_NON_LINE_MO
!====
!   DANS LE CAS TRES PARTICULIER DE LA COMMANDE THER_NON_LINE_MO
!   AVEC LA CHARGE : ECHANGE_PAROI, ON ENVOIE UNE CARTE DE TEMPS
!   OU THETA A ETE REMPLACE PAR 1-THETA
                    call getres(k8bid, k8bid, nomcmd)
                    if ((nomcmd.eq.'THER_NON_LINE_MO') .and. (option( 11:14).eq.'PARO')) then
                        lpain(3) = 'PTEMPSR'
                        lchin(3) = '&&OP0171.TIMEMO'
                    endif
!
!====
! 4.6 PRETRAITEMENTS POUR TENIR COMPTE DE FONC_MULT
!====
                    call gcnco2(newnom)
                    resuel(10:16) = newnom(2:8)
                    call corich('E', resuel, icha, ibid)
!
!====
! 4.7 CAS PARTICULIER X-FEM
!====
                    call exixfe(modele, iret)
                    if (iret .ne. 0) then
                        call xajcin(modele, option, nchinx, lchin, lpain,&
                                    nchin)
                        ligcal = ligrel(1)
                    else
                        ligcal = ligrel(nligr(k))
                    endif
!
!====
! 4.8 LANCEMENT DES CALCULS ELEMENTAIRES OPTION
!====
                    if (niv .eq. 2) then
                        write (ifm,*) '     K              :',k
                        write (ifm,*) '     OPTION         :',option
                        do i = 1, nchin
                            write (ifm,*) '     LPAIN/LCHIN    :',lpain(i),' ',&
     &              lchin(i)
                        end do
                    endif
                    call calcul('S', option, ligcal, nchin, lchin,&
                                lpain, 1, resuel, paout, 'V',&
                                'OUI')
!
! INCREMENTATION DE LONUTI ET STOCKAGE DU RESULTAT
                    ilve = ilve + 1
                    zk24(jlve-1+ilve) = resuel
!
                endif
            end do
!
!=====
! 4.10 FIN BOUCLE SUR LES NBCHMX CL NEUMANN POSSIBLES =======
!=====
!
! FIN TEST SUR NUMCHM
        endif
!
    end do
!
!====
! 6. FIN BOUCLE SUR LES CHARGES ========================================
!====
! MODIFICATION DU NBRE DE TERMES UTILES DU VECTEUR ELEM RESULTAT
    call jeecra(vecelz, 'LONUTI', ilve)
!
! SORTIE DE SECOURS EN CAS D'ABSENCE DE CHARGE
 80 continue
!
    call jedema()
end subroutine
