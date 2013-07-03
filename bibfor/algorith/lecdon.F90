subroutine lecdon(ficext, unitpa, prdeff)
    implicit          none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
#include "asterfort/wkvect.h"
    integer :: unitpa
    logical :: prdeff, ficext
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
! person_in_charge: nicolas.greffet at edf.fr
! **********************************************************************
! *   LOGICIEL CODE_ASTER - COUPLAGE ASTER/EDYOS - COPYRIGHT EDF 2009  *
! **********************************************************************
!  LECDON : FONCTION
!  -----------------
! CE SSP PERMET DE LIRE LES DONNEES RELATIVES AUX PALIERS CONTENUES DANS
!  LE FICHIER FIC_DON, SOIT:
!          - LE NOMBRE DE PALIERS
!          - (POUR CHAQUE PALIER) LE NOEUD ASTER D'APPLICATION ET
!          LE TYPE DE PALIER
!=======================================================================
!  REFERENCES BIBLIOGRAPHIQUES
!  ---------------------------
! ======================================================================
!  DEVELOPPEMENTS ET CORRECTIONS D'ANOMALIES
!  -----------------------------------------
!  DATE: 13/02/09   AUTEUR: P. VAUGRANTE    ANOMALIE: DEVELOPPEMENT
!  DATE:            AUTEUR:                 ANOMALIE:
!  DATE:            AUTEUR:                 ANOMALIE:
!  DATE:            AUTEUR:                 ANOMALIE:
! ======================================================================
!  VARIABLES UTILISEES
!  -------------------
!
!  ___________________________________________________________________
! !    NOM   !   TYPE      !                  ROLE                    !
! !__________!_____________!__________________________________________!
! !          !             !                                          !
! ! CARAC"I" !  CHARACTER  !  VARIABLE TAMPON PERMETTANT DE FAIRE     !
! !          !             !  CORRESPONDRE A UN NOMBRE SON EQUIVALENT !
! !          !             !  EN CARACERES                            !
! !          !             !                                          !
! ! UNITPA   !  ENTIER     !  UNITE DE LECTURE                        !
! !          !             !                                          !
! ! CTYPE    !  CHARACTER*6!  TYPE DU PALIER LU                       !
! !          !             !                                          !
! ! IPAL     !  ENTIER     !  INDICE DE BOUCLE SUR LES PALIERS        !
! !          !             !                                          !
! ! NUMPAL   !  ENTIER     !  NUMERO DU PALIER LU                     !
! !          !             !                                          !
! ! NOMPRG   !  CHARACTER  !  NOM DU SSP (POUR ECRITURE DANS ERRCOU)  !
! !          !             !                                          !
! ! PALMAX   !  ENTIER     !  NOMBRE MAXIMUM DE PALIERS (PARAMETER)   !
! !          !             !                                          !
! ! DIMNAS   !  CHARACTER  !  NOMBRE DE DDL POUR UN NOEUD             !
! !          !             !                                          !
! !__________!_____________!__________________________________________!
!
!
!
!
!
!
!  COMMON ZI (TYPE: INTEGER) (NOM = 'NPAL')
!  _____________________________________________________________________
! !              !             !                                       !
! ! NBPAL        !  ADR        !  NOMBRE DE PALIERS POUR L'ETUDE       !
! !              !             !                                       !
! !______________!_____________!_______________________________________!
!
!
!
!  COMMON ZK8 (TYPE: CHARACTER*8) (NOM = 'C_PAL')
!  _____________________________________________________________________
! !              !             !                                       !
! ! TYPPAL(IPAL) ! ADR+(IPAL-1)!  TYPE DU PALIER CONSIDERE             !
! !              !             !                                       !
! ! FINPAL(IPAL) !  ADR+PALMAX !  TERMINAISON POUR LE PALIER CONSIDERE !
! !              !  +(IPAL-1)  !  PALIER N°I => _I                     !
! !              !             !                                       !
! ! CNPAL(IPAL)  ! ADR+2*PALMAX!  NOM DU NOEUD ASTER POUR LE PALIER    !
! !              !  +(IPAL-1)  !  CONSIDERE                            !
! !______________!_____________!_______________________________________!
!
!
!
!
!
!
!     VARIABLES INTERNES
!     ==================
    integer :: ifm, niv
    character(len=8) :: nomprg
    parameter(nomprg='LECDON')
!
    character(len=1) :: carac1
    character(len=2) :: carac2
!
    integer :: ipal, numpal, ngr, n2
    integer :: nbpal, iret
    character(len=6) :: ctype
!
    integer :: palmax
    parameter (palmax=20)
    character(len=6) :: typpal(palmax)
    character(len=3) :: finpal(palmax)
    character(len=8) :: cnpal(palmax), cnod, k8b
!
    integer :: zcpal, znpal
    character(len=16) :: k16nom
    character(len=24) :: cpal, npal
    integer :: iarg
!
    call jemarq()
    niv = 0
    call infdbg('YACS_EDYOS', ifm, niv)
!
!     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
!     ------------------------------------------------------------
    cpal='C_PAL'
    npal='N_PAL'
!
!     RESERVATION MEMOIRE POUR LES "COMMON"  ASTER
!     --------------------------------------------
    call jeexin(cpal, iret)
    if (iret .eq. 0) then
        call wkvect(cpal, 'G V K8', (3*palmax), zcpal)
    else
        call jeveuo(cpal, 'E', zcpal)
    endif
    call jeexin(npal, iret)
    if (iret .eq. 0) then
        call wkvect(npal, 'G V I', (1+palmax), znpal)
    else
        call jeveuo(npal, 'E', znpal)
    endif
!
    if (ficext) then
!
!     LECTURE DU FICHIER FIC_DON
!     --------------------------
        if (niv .ge. 2) write(ifm, *)'ASTEREDYOS: ', nomprg,&
                        ' DEBUT LECTURE DU FICHIER FIC_DON UNITE LOGIQUE ', unitpa
        k16nom ='                '
        if (ulisop ( unitpa, k16nom ) .eq. 0) then
            call ulopen(unitpa, ' ', ' ', 'NEW', 'O')
        endif
        read(unitpa,*)nbpal
        if (niv .ge. 2) write(ifm, *)'ASTEREDYOS: ', nomprg, ' ON A LU NBPAL =', nbpal
        if (nbpal .gt. palmax) call u2mess('F', 'EDYOS_43')
!
!     REMPLISSAGE "COMMON" ASTER POUR LE NOMBRE DE PALIERS
!     ----------------------------------------------------
        zi(znpal)=nbpal
!
!     BOUCLE DE LECTURE SUR LES PALIERS
!     ---------------------------------
        do 100 ipal = 1, nbpal
            cnod='        '
            read(unitpa,*)numpal,cnod,ctype
            typpal(numpal)=ctype
            cnpal(ipal)=cnod
!
            if (ipal .lt. 10) then
                write(carac1,'(I1)')ipal
                finpal(ipal)='_'//carac1
            else
                write(carac2,'(I2)')ipal
                finpal(ipal)='_'//carac2
            endif
!
!
!   REMPLISSAGE "COMMON" ASTER POUR LES PALIERS (TYPE,TERMINAISON,NOEUD)
!   --------------------------------------------------------------------
            zk8(zcpal+(ipal-1))=ctype
            zk8(zcpal+palmax+(ipal-1))=finpal(ipal)
            zk8(zcpal+(2*palmax)+(ipal-1))=cnpal(ipal)
            if (niv .ge. 2) write(ifm, * )'ASTEREDYOS : LECDON : CTYPE - FINPAL - CNPAL=', ctype,&
                            ' -- ', finpal(ipal), ' -- ', cnpal(ipal)
!
!   REMPLISSAGE "COMMON" ASTER POUR LES NUMEROS DES NOEUDS DES PALIERS
!   ------------------------------------------------------------------
            zi(znpal+1+(ipal-1))=ipal
100      end do
!
!     FIN DE BOUCLE DE LECTURE SUR LES PALIERS
!     ----------------------------------------
        prdeff = .true.
!
!     ECRITURE DES VARIABLES LUES
!     ---------------------------
        if (niv .ge. 2) then
            write(ifm,*)'ASTEREDYOS: ',nomprg,&
     &              ' - FIN LECTURE DU FICHIER FIC_DON '
            write(ifm,*)'ASTEREDYOS: ',nomprg,' - NOMBRE DE PALIERS: ',nbpal
            do 110 ipal = 1, nbpal
                write(ifm,*)'ASTEREDYOS PALIER :',ipal,' TYPE :',&
                typpal(ipal), ' NOEUD ASTER : ',cnpal(ipal)
110          continue
        endif
!
        if (ulisop ( unitpa, k16nom ) .ne. 0) call ulopen(-unitpa, ' ', ' ', 'NEW', 'O')
!
    else
!
!   LECTURE DES INFOS DEPUIS LE FICHIER DE COMMANDE
!   --------------------------------------------------------------------
        call getfac('PALIER_EDYOS', nbpal)
        if (nbpal .gt. palmax) call u2mess('F', 'EDYOS_43')
        zi(znpal)=nbpal
        do 201 ipal = 1, nbpal
            call getvtx('PALIER_EDYOS', 'GROUP_NO', ipal, iarg, 0,&
                        k8b, n2)
            if (abs(n2) .eq. 0) then
                call getvtx('PALIER_EDYOS', 'NOEUD', ipal, iarg, 0,&
                            k8b, n2)
                if (abs(n2) .eq. 0) then
                    call u2mess('F', 'EDYOS_49')
                else
                    ngr = -n2
                    ngr = 1
                    call getvtx('PALIER_EDYOS', 'NOEUD', ipal, iarg, ngr,&
                                cnpal(ipal), n2)
                endif
            else
                ngr = -n2
                ngr = 1
                call getvtx('PALIER_EDYOS', 'GROUP_NO', ipal, iarg, ngr,&
                            cnpal(ipal), n2)
            endif
            call getvtx('PALIER_EDYOS', 'TYPE_EDYOS', ipal, iarg, ngr,&
                        ctype, n2)
            zk8(zcpal+(2*palmax)+(ipal-1))=cnpal(ipal)
            zk8(zcpal+(ipal-1))=ctype
201      continue
!
        niv = 3
!
        do 101 ipal = 1, nbpal
!
            if (ipal .lt. 10) then
                write(carac1,'(I1)')ipal
                finpal(ipal)='_'//carac1
            else
                write(carac2,'(I2)')ipal
                finpal(ipal)='_'//carac2
            endif
            zk8(zcpal+palmax+(ipal-1))=finpal(ipal)
            if (niv .ge. 2) write(ifm, * )'ASTEREDYOS : LECDON : CTYPE - FINPAL - CNPAL=',&
                            zk8(zcpal+(ipal-1)), ' -- ', finpal(ipal), ' -- ',&
                            zk8(zcpal+(2*palmax)+(ipal-1))
            zi(znpal+1+(ipal-1))=ipal
101      continue
        if (niv .ge. 2) then
            write(ifm,*)'ASTEREDYOS: ',nomprg,&
     &              ' - FIN LECTURE ARGUMENTS PALIERS '
            write(ifm,*)'ASTEREDYOS: ',nomprg,' - NOMBRE PALIERS: ',&
            nbpal
        endif
    endif
!
!
    call jedema()
!
end subroutine
